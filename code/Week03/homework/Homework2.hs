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
mkParameterizedVestingValidator _beneficiary _deadline () ctx =
    (traceIfFalse "beneficiary's signature missing" $ isSigned _beneficiary)
    &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    ctxInfo :: TxInfo
    ctxInfo = scriptContextTxInfo ctx

    isSigned :: PubKeyHash -> Bool
    isSigned = txSignedBy ctxInfo

    deadlineReached :: Bool
    deadlineReached = (from $ _deadline) `contains` txInfoValidRange ctxInfo
    --deadlineReached = _deadline `before` txInfoValidRange ctxInfo  -- BUG: not working
    --deadlineReached = (flip after (txInfoValidRange ctxInfo)) _deadline -- BUG: not working

{-# INLINABLE  e #-}
e :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
e = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| e ||]) <**> liftCode beneficiary)
  where
    (<**>) = applyCode
