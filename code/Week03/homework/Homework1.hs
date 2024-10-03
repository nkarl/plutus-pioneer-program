{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (before, after)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo),
                                       TxInfo (txInfoValidRange), from,
                                       Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if
--  - either  beneficiary1 has signed the transaction and the current slot is before or at the deadline
--  - or      beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator contractDetails () ctx =
    ((traceIfFalse "beneficiary 1's signature missing" $ isSigned $ beneficiary1 contractDetails)
    &&
    traceIfFalse "deadline is passed" isBeforeDeadline)
    ||
    ((traceIfFalse "beneficiary 2's signature missing" $ isSigned $ beneficiary2 contractDetails)
    &&
    traceIfFalse "deadline is not reached" isAfterDeadline)
  where
    finalDate = deadline contractDetails
    validRange = txInfoValidRange info

    info :: TxInfo
    info = scriptContextTxInfo ctx

    isSigned :: PubKeyHash -> Bool
    isSigned k = txSignedBy info $ k

    isBeforeDeadline :: Bool
    isBeforeDeadline = finalDate `after` validRange

    isAfterDeadline :: Bool
    isAfterDeadline = finalDate `before` validRange


{-# INLINABLE  e #-}
e :: BuiltinData -> BuiltinData -> BuiltinData -> ()
e = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| e ||])
