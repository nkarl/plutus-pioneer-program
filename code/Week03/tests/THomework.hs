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

type Homework1Script = TypedValidator H1.VestingDatum ()
--
-- NOTE: 0. contract is already written.
addrCtr1 :: Homework1Script
addrCtr1 = TypedValidator $ toV2 H1.validator

{-
 - TODO: try to change the logic of test suite and validator script to address the following questions
 - [ ] Why are the POSIXTime input are negative? They don't match the valid range in the test's title.
 - [ ] Why is fund claim ends with a tx signed by the Giver's sig? Isn't it supposed to signed with the Taker's sig?
-}

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1"
    [ testGroup
        "Stakeholder 1 signing"
        [ good "Deadline: 6000; TxValidRange (5000, 5999)" $ testStakeholder1 6000 (-999) 0 0
        , good "Deadline: 6000; TxValidRange (5000, 6000)" $ testStakeholder1 6000 (-999) 1 0
        , good "Deadline: 6000; TxValidRange (5000, 6999)" $ testStakeholder1 6000 (-999) 1000 0
        , good "Deadline: 6000; TxValidRange (5999, 6001)" $ testStakeholder1 6000 0 2 0
        , good "Deadline: 6000; TxValidRange (6999, 6999)" $ testStakeholder1 6000 0 0 1
        , bad  "Deadline: 6000; TxValidRange (7000, 8000)" $ testStakeholder1 6000 1 1001 1
        , bad  "Deadline: 6000; TxValidRange (5000, 7000)" $ testStakeholder1 6000 (-999) 1001 0
        , bad  "Deadline: 6000; TxValidRange (6000, 7000)" $ testStakeholder1 6000 (-999) 1 1
        , bad  "Deadline: 6000; TxValidRange (6999, 7000)" $ testStakeholder1 6000 0 1 1
        ]
    , testGroup
        "Stakeholder 2 signing"
        [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testStakeholder2 5000 (-999) 1 1
        , good "Deadline: 4999; TxValidRange (5000, 6000)" $ testStakeholder2 4999 (-999) 1 0
        , bad  "Deadline: 6000; TxValidRange (5000, 5999)" $ testStakeholder2 6000 (-999) 0 0
        , bad  "Deadline: 5000; TxValidRange (5000, 6000)" $ testStakeholder2 5000 (-999) 1 0
        , bad  "Deadline: 5000; TxValidRange (5001, 6000)" $ testStakeholder2 5000 (-998) 1 0
        , bad  "Deadline: 5000; TxValidRange (5999, 6000)" $ testStakeholder2 5000 0 1 0
        ]
    , bad "None signing" $ testNoSigning 5000 0 0 0 -- fails if no sigs are found
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

--                  deadline     start        end
testStakeholder1 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()   -- NOTE first battery: Giver sig + Contract addr.
testStakeholder1 deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users  -- [ giver, contract, taker ]
      ctrRequirements = H1.VestingDatum u1 u2 deadline
  testHomework1 u1 u3 ctrRequirements startT endT wSlot

testStakeholder2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()   -- NOTE second battery: Contract addr + Taker sig.
testStakeholder2 deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users  -- [ giver, contract, taker ]
      ctrRequirements = H1.VestingDatum u1 u2 deadline
  testHomework1 u2 u3 ctrRequirements startT endT wSlot

testNoSigning :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()      -- NOTE third battery: not (Contract addr + Stakeholder sig).
testNoSigning deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users  -- [ giver, contract, taker ]
      ctrRequirements = H1.VestingDatum u1 u2 deadline
  testHomework1 u3 u3 ctrRequirements startT endT wSlot

testHomework1 :: PubKeyHash -> PubKeyHash -> H1.VestingDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework1 giverSig takerSig ctrRequirements startT endT wSlot = do

  -- NOTE: 1. Giver u1 sets up the Contract u2.
  let amount          = adaValue 100
      ctr             = addrCtr1
      contractBalance = giverSig `gives` amount $ ctr
  checkBalance contractBalance $ do
    spendingAction <- giverSig `spend` amount                               -- NOTE creates a spending action, and
    giverSig `submitTx` (vestingTx1 ctrRequirements spendingAction amount)  -- NOTE submit Tx with Giver's sig.

  waitNSlots wSlot -- NOTE hidden time-cost (assuming some time needs to pass before u3 acts on claiming).
  -- additional note: related to the `k` of the consensus algorithm. This is a mock config so we don't need to wait.

  -- NOTE: 2. Taker u3 attempts to claim the Contract u2.
  utxos <- utxoAt ctr                                                       -- NOTE the Taker looks up the utxos at the Contract address.
  let [(vestRef, vestOut)] = utxos
      claimBalance = ctr `gives` (txOutValue vestOut) $ takerSig            -- NOTE defines the action to spend/give the Contract to the Taker

  checkBalance claimBalance $ do
    range <- currentTimeInterval startT endT                                -- NOTE wraps the input POSIXTime into a range
    tx <- validateIn range                                                  -- NOTE: validates the Contract.
              $ claimingTx1
                  takerSig ctrRequirements vestRef (txOutValue vestOut)     -- NOTE invokes a claiming action to conclude the Contract.
    giverSig `submitTx` tx                                                  -- NOTE finalizes the tx by signing with Giver's sig.
    -- Possible BUG: shouldn't the tx be signed with sig of the Taker?

vestingTx1 :: H1.VestingDatum -> UserSpend -> Value -> Tx                   -- NOTE defines the vesting part of the contract
vestingTx1 ctrRequirements usp amount =
  mconcat -- the eleemnts are folded into a single Tx.
    [ userSpend usp
    , payToScript addrCtr1 (HashDatum ctrRequirements) amount
    ]

claimingTx1 :: PubKeyHash -> H1.VestingDatum -> TxOutRef -> Value -> Tx     -- NOTE defines the claiming part of the contract
claimingTx1 pkh ctrRequirements vestRef vestAmount =
  mconcat -- the elements are folded into a single Tx.
    [ spendScript addrCtr1 vestRef () ctrRequirements
    , payToKey pkh vestAmount
    ]

type Homework2Script = TypedValidator POSIXTime ()

addrCtr2 :: PubKeyHash -> Homework2Script
addrCtr2 = TypedValidator . toV2 . H2.validator

homework2 :: MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2"
    [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testHomework2 5000 (-999) 1 1
    , good "Deadline: 5000; TxValidRange (5000, 5000)" $ testHomework2 5000 (-999) (-999) 0
    , good "Deadline: 5000; TxValidRange (5000, 6000)" $ testHomework2 5000 (-999) 1 0
    , good "Deadline: 5000; TxValidRange (5001, 6000)" $ testHomework2 5000 (-998) 1 0
    , good "Deadline: 5000; TxValidRange (5999, 6000)" $ testHomework2 5000 0 1 0
    , bad  "Deadline: 5000; TxValidRange (4000, 5000)" $ testHomework2 5000 (-1999) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5000)" $ testHomework2 5000 (-1000) (-999) 0
    , bad  "Deadline: 5000; TxValidRange (4999, 5999)" $ testHomework2 5000 (-1000) 0 0
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

testHomework2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework2 ctrRequirements startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      amount = adaValue 100
      contractBalance = u1 `gives` amount $ addrCtr2 u2

  checkBalance contractBalance $ do
    spendingAction <- u1 `spend` amount
    u1 `submitTx` (vestingTx2 u2 ctrRequirements spendingAction amount)
  waitNSlots wSlot
  utxos <- utxoAt $ addrCtr2 u2
  let [(vestRef, vestOut)] = utxos
      claimBalance      = (addrCtr2 u2) `gives` (txOutValue vestOut) $ u2
  checkBalance claimBalance $ do
    range <- currentTimeInterval startT endT
    tx <- validateIn range
              $ claimingTx2
                  u2 ctrRequirements vestRef (txOutValue vestOut)
    u2 `submitTx` tx

vestingTx2 :: PubKeyHash -> POSIXTime -> UserSpend -> Value -> Tx
vestingTx2 pkh ctrRequirements usp amount =
  mconcat
    [ userSpend usp
    , payToScript (addrCtr2 pkh) (HashDatum ctrRequirements) amount
    ]

claimingTx2 :: PubKeyHash -> POSIXTime -> TxOutRef -> Value -> Tx
claimingTx2 takerSig ctrRequirements vestRef vestAmount =
  mconcat
    [ spendScript (addrCtr2 takerSig) vestRef () ctrRequirements
    , payToKey takerSig vestAmount
    ]
