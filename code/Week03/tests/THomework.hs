{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import           Control.Monad                 (replicateM)
import           Plutus.Model
import           Plutus.Model.Fork.Ledger.Slot (Slot)
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty
import qualified Homework1                     as H1
import qualified Homework2                     as H2


main :: IO ()
main = do
  defaultMain $ do
    testGroup "Homework tests"
      [
        testGroup "All times are in POSIXTime (Not slots)"
          [ homework1 defaultBabbage
          , homework2 defaultBabbage
          ]
      ]

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

type HW1Script = TypedValidator H1.VestingDatum ()

-- NOTE 0. contract is already written.
validator1 :: HW1Script
validator1 = TypedValidator $ toV2 H1.validator

{-
 - TODO: try to change the logic of test suite and validator script to address the following questions
 - [ ] Why are the POSIXTime input are negative? They don't match the valid range in the test's title.
 - [ ] Why is fund claim ends with a tx signed by the Giver's sig? Shouldn't it be signed with the Taker's sig?
-}

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1"
    [ testGroup
        "Giver signs and spends the Contract on Taker."
        [ good "Deadline: 6000; TxValidRange (5000, 5999)" $ testHW1_giverSpends 6000 (-999)    0  0
        , good "Deadline: 6000; TxValidRange (5000, 6000)" $ testHW1_giverSpends 6000 (-999)    1  0
        , good "Deadline: 6000; TxValidRange (5000, 6999)" $ testHW1_giverSpends 6000 (-999) 1000  0
        , good "Deadline: 6000; TxValidRange (5999, 6001)" $ testHW1_giverSpends 6000     0     2  0
        , good "Deadline: 6000; TxValidRange (6999, 6999)" $ testHW1_giverSpends 6000     0     0  1
        , bad  "Deadline: 6000; TxValidRange (7000, 8000)" $ testHW1_giverSpends 6000     1  1001  1
        , bad  "Deadline: 6000; TxValidRange (5000, 7000)" $ testHW1_giverSpends 6000 (-999) 1001  0
        , bad  "Deadline: 6000; TxValidRange (6000, 7000)" $ testHW1_giverSpends 6000 (-999)    1  1
        , bad  "Deadline: 6000; TxValidRange (6999, 7000)" $ testHW1_giverSpends 6000     0     1  1
        ]
    , testGroup
        "Taker spends the Contract (already signed by Giver)."
        [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testHW1_takerClaims 5000 (-999)    1  1
        , good "Deadline: 4999; TxValidRange (5000, 6000)" $ testHW1_takerClaims 4999 (-999)    1  0
        , bad  "Deadline: 6000; TxValidRange (5000, 5999)" $ testHW1_takerClaims 6000 (-999)    0  0
        , bad  "Deadline: 5000; TxValidRange (5000, 6000)" $ testHW1_takerClaims 5000 (-999)    1  0
        , bad  "Deadline: 5000; TxValidRange (5001, 6000)" $ testHW1_takerClaims 5000 (-998)    1  0
        , bad  "Deadline: 5000; TxValidRange (5999, 6000)" $ testHW1_takerClaims 5000     0     1  0
        ]
    , bad "None signing" $ testHW1_noSigning 5_000 0 0 0 -- fails if no sigs are found
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

-- NOTE: test function for Redeemer 1 (Giver)
-- 1. Giver sets up and signs Contract.
-- 2. the spending process has access to PKHs of Giver and Contract, and Datum.
-- 3. the spending process signs with PKH of Giver, and spends the Contract on Taker.
testHW1_giverSpends :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHW1_giverSpends deadline startT endT wSlot = do
  users <- setupUsers
  let [giver, contract, taker] = users
      datum = H1.VestingDatum giver contract deadline
  testHW1 giver taker datum startT endT wSlot

-- NOTE: test function for Redeemer 2 (Taker)
-- 1. Giver sets up and signs Contract.
-- 2. the spending process has access to PKH of Contract and Datum.
-- 3. the spending process signs with PKH of Contract, and spends the Contract on Taker.
testHW1_takerClaims :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHW1_takerClaims deadline startT endT wSlot = do
  users <- setupUsers
  let [giver, contract, taker] = users
      datum = H1.VestingDatum giver contract deadline
  testHW1 contract taker datum startT endT wSlot

-- NOTE: test function for incorrect number of signatures.
-- Taker has access to nothing except for Datum.
testHW1_noSigning :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHW1_noSigning deadline startT endT wSlot = do
  users <- setupUsers
  let [giver, contract, taker] = users
      datum = H1.VestingDatum giver contract deadline
  testHW1 taker taker datum startT endT wSlot

-- NOTE: tests the use cases for HW1
testHW1 :: PubKeyHash -> PubKeyHash -> H1.VestingDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHW1 redeemerGiver redeemerTaker datum startT endT wSlot = do

  -- NOTE 1. Giver sets up the Contract by making a new UTxO and attach the validator script
  let amount = adaValue 100
      ctrBalance = redeemerGiver `gives` amount $ validator1
  checkBalance ctrBalance $ do
    spendAction <- redeemerGiver `spend` amount                 -- NOTE creates a spending action, and
    redeemerGiver `submitTx` vestingTx1 spendAction amount      -- NOTE submit Tx with Giver's sig.

  -- NOTE hidden time-cost (assuming some time needs to pass before taker acts on claiming).
  waitNSlots wSlot
  -- additional note: related to the `k` of the consensus algorithm. This is a mock config so we don't need to wait.

  -- NOTE 2. Taker attempts to spend the Contract at the validator.
  utxos <- utxoAt validator1                                    -- NOTE the Taker looks up the utxos at the Contract address.
  let [(vestRef, vestOut)] = utxos
      claimBalance =
        validator1 `gives` txOutValue vestOut $ redeemerTaker   -- NOTE defines the action to spend/give the Contract to the Taker

  checkBalance claimBalance $ do
    range <- currentTimeInterval startT endT                    -- NOTE wraps the input POSIXTime into a range
    tx <- validateIn range                                      -- NOTE: validates the Contract.
              $ claimingTx1
                  redeemerTaker vestRef (txOutValue vestOut)    -- NOTE invokes a claiming action to conclude the Contract.
    redeemerGiver `submitTx` tx                                 -- NOTE finalizes the tx by signing with Giver's sig.

  where
  -- NOTE: composes the Vesting Tx
  vestingTx1 :: UserSpend -> Value -> Tx 
  vestingTx1 spendAction amount =
    mconcat -- the eleemnts are folded into a single Tx.
      [ userSpend spendAction
      , payToScript validator1 (HashDatum datum) amount
      ]

  -- NOTE: composes the Spending Tx
  claimingTx1 :: PubKeyHash -> TxOutRef -> Value -> Tx
  claimingTx1 pkh vestRef vestAmount =
    mconcat -- the elements are folded into a single Tx.
      [ spendScript validator1 vestRef () datum
      , payToKey pkh vestAmount
      ]


type HW2Script = TypedValidator POSIXTime ()

validator2 :: PubKeyHash -> HW2Script
validator2 = TypedValidator . toV2 . H2.validator

homework2 :: MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2"
    [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testHW2 5000 (-999) 1 1
    , good "Deadline: 5000; TxValidRange (5000, 5000)" $ testHW2 5000 (-999) (-999) 0
    , good "Deadline: 5000; TxValidRange (5000, 6000)" $ testHW2 5000 (-999) 1 0
    , good "Deadline: 5000; TxValidRange (5001, 6000)" $ testHW2 5000 (-998) 1 0
    , good "Deadline: 5000; TxValidRange (5999, 6000)" $ testHW2 5000 0 1 0
    , bad  "Deadline: 5000; TxValidRange (4000, 5000)" $ testHW2 5000 (-1999) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5000)" $ testHW2 5000 (-1000) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5999)" $ testHW2 5000 (-1000) 0 0
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

testHW2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHW2 datum startT endT wSlot = do
  users <- setupUsers
  let [giver, contract, _u3] = users
      amount = adaValue 100
      ctrBalance = giver `gives` amount $ validator2 contract

  checkBalance ctrBalance $ do
    spendingAction <- giver `spend` amount
    giver `submitTx` vestingTx2 contract datum spendingAction amount

  waitNSlots wSlot

  utxos <- utxoAt $ validator2 contract
  let [(vestRef, vestOut)] = utxos
      claimBalance = validator2 contract `gives` txOutValue vestOut $ contract
  checkBalance claimBalance $ do
    range <- currentTimeInterval startT endT
    tx <- validateIn range
              $ claimingTx2
                  contract datum vestRef (txOutValue vestOut)
    contract `submitTx` tx

vestingTx2 :: PubKeyHash -> POSIXTime -> UserSpend -> Value -> Tx
vestingTx2 pkh ctrRequirements usp amount =
  mconcat
    [ userSpend usp
    , payToScript (validator2 pkh) (HashDatum ctrRequirements) amount
    ]

claimingTx2 :: PubKeyHash -> POSIXTime -> TxOutRef -> Value -> Tx
claimingTx2 redeemerTaker datum vestRef vestAmount =
  mconcat
    [ spendScript (validator2 redeemerTaker) vestRef () datum
    , payToKey redeemerTaker vestAmount
    ]
