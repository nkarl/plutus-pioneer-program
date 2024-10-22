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

type Homework1Script = TypedValidator H1.VestingDatum ()

addrCtx1 :: Homework1Script
addrCtx1 = TypedValidator $ toV2 H1.validator

type Homework2Script = TypedValidator POSIXTime ()

addrCtx2 :: PubKeyHash -> Homework2Script
addrCtx2 = TypedValidator . toV2 . H2.validator

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada (Lovelace 1000)

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

homework1 :: MockConfig -> TestTree
homework1 cfg = do
  testGroup
    "Testing Homework1"
    [ testGroup
        "Beneficiary 1 signing"
        [ good "Deadline: 6000; TxValidRange (5000, 5999)" $ testBeneficiary1 6000 (-999) 0 0
        , good "Deadline: 6000; TxValidRange (5000, 6000)" $ testBeneficiary1 6000 (-999) 1 0
        , good "Deadline: 6000; TxValidRange (5000, 6999)" $ testBeneficiary1 6000 (-999) 1000 0
        , good "Deadline: 6000; TxValidRange (5999, 6001)" $ testBeneficiary1 6000 0 2 0
        , good "Deadline: 6000; TxValidRange (6999, 6999)" $ testBeneficiary1 6000 0 0 1
        , bad  "Deadline: 6000; TxValidRange (7000, 8000)" $ testBeneficiary1 6000 1 1001 1
        , bad  "Deadline: 6000; TxValidRange (5000, 7000)" $ testBeneficiary1 6000 (-999) 1001 0
        , bad  "Deadline: 6000; TxValidRange (6000, 7000)" $ testBeneficiary1 6000 (-999) 1 1
        , bad  "Deadline: 6000; TxValidRange (6999, 7000)" $ testBeneficiary1 6000 0 1 1
        ]
    , testGroup
        "Beneficiary 2 signing"
        [ good "Deadline: 5000; TxValidRange (6000, 7000)" $ testBeneficiary2 5000 (-999) 1 1
        , good "Deadline: 4999; TxValidRange (5000, 6000)" $ testBeneficiary2 4999 (-999) 1 0
        , bad  "Deadline: 6000; TxValidRange (5000, 5999)" $ testBeneficiary2 6000 (-999) 0 0
        , bad  "Deadline: 5000; TxValidRange (5000, 6000)" $ testBeneficiary2 5000 (-999) 1 0
        , bad  "Deadline: 5000; TxValidRange (5001, 6000)" $ testBeneficiary2 5000 (-998) 1 0
        , bad  "Deadline: 5000; TxValidRange (5999, 6000)" $ testBeneficiary2 5000 0 1 0
        ]
    , bad "None signing" $ testNoSigning 5000 0 0 0 -- fails if no sigs are found
    ]
  where
    bad msg = good msg . mustFail
    good = testNoErrors (adaValue 10_000_000) cfg

--                  deadline     start        end
testBeneficiary1 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()   -- NOTE first battery: spender signed.
testBeneficiary1 deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users  -- [ giver, contract, claimant ]
      contractInfo = H1.VestingDatum u1 u2 deadline
  testHomework1 u1 u3 contractInfo startT endT wSlot

testBeneficiary2 :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()   -- NOTE second battery: claimant signed.
testBeneficiary2 deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3] = users  -- [ giver, contract, claimant ]
      contractInfo = H1.VestingDatum u1 u2 deadline
  testHomework1 u2 u3 contractInfo startT endT wSlot

testNoSigning :: POSIXTime -> POSIXTime -> POSIXTime -> Slot -> Run ()      -- NOTE third battery: no signature from either stakeholders.
testNoSigning deadline startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, u3]  = users  -- [ giver, contract, claimant ]
      contractInfo = H1.VestingDatum u1 u2 deadline
  testHomework1 u3 u3 contractInfo startT endT wSlot

testHomework1 :: PubKeyHash -> PubKeyHash -> H1.VestingDatum -> POSIXTime -> POSIXTime -> Slot -> Run ()
testHomework1 patronSig claimantSig contractInfo startT endT wSlot = do

  -- NOTE: 1. Spender sets up the contract.
  let amount          = adaValue 100
      contractBalance = gives patronSig amount addrCtx1
  checkBalance contractBalance $ do
    sp <- spend patronSig amount                            -- NOTE creates a spending action, and
    submitTx patronSig (vestingTx1 contractInfo sp amount)  -- NOTE submit Tx with patron's sig.

  -- NOTE hidden time-cost (assuming some time has to pass first before the claimant acts).
  -- additional note: seems to be related to the `k` of the consensus algorithm. This is a mock so we don't need to wait.
  waitNSlots wSlot

  -- NOTE: 2. Claimant attempts to claim the contract.
  utxos <- utxoAt addrCtx1                       -- NOTE now the claimant looks up the utxo at the contract address.
  let [(vestRef, vestOut)]  = utxos
      claimantBalance       = gives                   -- NOTE defines the action to claim/give the contract
                                addrCtx1
                                (txOutValue vestOut)
                                claimantSig

  checkBalance claimantBalance $ do
    range <- currentTimeInterval startT endT          -- NOTE evaluates the current valid range
    tx <- validateIn range                            -- NOTE: contract validation.
              $ claimingTx1                           -- NOTE defines a claiming action to conclude the contract.
                  claimantSig
                  contractInfo
                  vestRef
                  (txOutValue vestOut)
    submitTx patronSig tx                             -- NOTE finalizes the tx by signing with patron's sig.

vestingTx1 :: H1.VestingDatum -> UserSpend -> Value -> Tx                 -- NOTE defines the vesting part of the contract
vestingTx1 contractInfo usp amount =
  mconcat -- the eleemnts are folded into a single Tx.
    [ userSpend usp
    , payToScript addrCtx1 (HashDatum contractInfo) amount
    ]

claimingTx1 :: PubKeyHash -> H1.VestingDatum -> TxOutRef -> Value -> Tx   -- NOTE defines the claiming part of the contract
claimingTx1 key contractInfo vestRef vestAmount =
  mconcat -- the elements are folded into a single Tx.
    [ spendScript addrCtx1 vestRef () contractInfo
    , payToKey key vestAmount
    ]


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
testHomework2 contractInfo startT endT wSlot = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      amount = adaValue 100

  checkBalance (gives u1 amount $ addrCtx2 u2) $ do
    spending <- spend u1 amount
    submitTx u1 $ vestingTx2 u2 contractInfo spending amount
  waitNSlots wSlot
  utxos <- utxoAt $ addrCtx2 u2
  let [(vestRef, vestOut)] = utxos
  checkBalance (gives (addrCtx2 u2) (txOutValue vestOut) u2) $ do
    range <- currentTimeInterval startT endT
    tx <- validateIn range $ claimingTx2 u2 contractInfo vestRef (txOutValue vestOut)
    submitTx u2 tx

vestingTx2 :: PubKeyHash -> POSIXTime -> UserSpend -> Value -> Tx
vestingTx2 key contractInfo usp amount =
  mconcat
    [ userSpend usp
    , payToScript (addrCtx2 key) (HashDatum contractInfo) amount
    ]

claimingTx2 :: PubKeyHash -> POSIXTime -> TxOutRef -> Value -> Tx
claimingTx2 claimantSig contractInfo vestRef vestAmount =
  mconcat
    [ spendScript (addrCtx2 claimantSig) vestRef () contractInfo
    , payToKey claimantSig vestAmount
    ]
