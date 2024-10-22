{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V1.Ledger.Interval (contains)--, after, before)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext (scriptContextTxInfo), Validator,
                                       TxInfo (txInfoValidRange),
                                       from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (applyCode, compile, liftCode, makeLift)
import           PlutusTx.Prelude     (Bool (..), (.), ($), (&&), traceIfFalse)
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

-- data Requirements = Requirements
data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

-- makeLift ''Requirements
makeLift ''VestingDatum

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has
--  - both  a signature from the parameterized beneficiary
--  - and   the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator _beneficiary _deadline () _ctx =
    (traceIfFalse "beneficiary's signature missing" $ isSigned _beneficiary)
    &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    isSigned :: PubKeyHash -> Bool
    isSigned = txSignedBy info

    deadlineReached :: Bool
    deadlineReached = (from $ _deadline) `contains` txInfoValidRange info
    --deadlineReached = _deadline `before` txInfoValidRange info  -- BUG: not working
    --deadlineReached = (flip after (txInfoValidRange info)) _deadline -- BUG: not working

{-# INLINABLE  e #-}
e :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
e = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| e ||]) `applyCode` liftCode beneficiary)
