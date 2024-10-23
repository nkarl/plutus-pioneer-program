{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (before, after)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTimeRange, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo),
                                       TxInfo (txInfoValidRange),
                                       Validator, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), traceIfFalse, ($), (&&), (||), flip, (.))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- data Requirements = Requirements
data VestingDatum = VestingDatum
    { stakeholder1 :: PubKeyHash
    , stakeholder2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

-- unstableMakeIsData ''Requirements
unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
-- This should validate if
--  - either  stakeholder1 has signed the transaction and the current slot is before or at the deadline
--  - or      stakeholder2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator contract () ctx =
    ((traceIfFalse "stakeholder 1's signature missing" $ isSigned $ stakeholder1 contract)
        && traceIfFalse "deadline is passed" (isNotAfterDeadline contract))
    ||
    ((traceIfFalse "stakeholder 2's signature missing" $ isSigned $ stakeholder2 contract)
        && traceIfFalse "deadline is not reached" (isAfterDeadline contract))
  where
    ctxInfo :: TxInfo
    ctxInfo = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange ctxInfo

    isSigned :: PubKeyHash -> Bool
    isSigned = txSignedBy ctxInfo

    isNotAfterDeadline :: VestingDatum -> Bool
    isNotAfterDeadline = (flip after range) . deadline

    isAfterDeadline :: VestingDatum -> Bool
    isAfterDeadline = (flip before range) . deadline


{-# INLINABLE  e #-}
e :: BuiltinData -> BuiltinData -> BuiltinData -> ()
e = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| e ||])
