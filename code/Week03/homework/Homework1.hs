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

-- data State = State
data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

-- unstableMakeIsData ''State
unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
-- This should validate if
--  - either  beneficiary1 has signed the transaction and the current slot is before or at the deadline
--  - or      beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator contract () ctx =
    ((traceIfFalse "beneficiary 1's signature missing" $ isSigned $ beneficiary1 contract)
        &&
        traceIfFalse "deadline is passed" (isNotAfterDeadline contract)
      )
    ||
    ((traceIfFalse "beneficiary 2's signature missing" $ isSigned $ beneficiary2 contract)
        &&
        traceIfFalse "deadline is not reached" (isAfterDeadline contract)
      )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validRange :: POSIXTimeRange
    validRange = txInfoValidRange info

    isSigned :: PubKeyHash -> Bool
    isSigned = txSignedBy info

    isNotAfterDeadline :: VestingDatum -> Bool
    isNotAfterDeadline = (flip after validRange) . deadline

    isAfterDeadline :: VestingDatum -> Bool
    isAfterDeadline = (flip before validRange) . deadline


{-# INLINABLE  e #-}
e :: BuiltinData -> BuiltinData -> BuiltinData -> ()
e = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| e ||])
