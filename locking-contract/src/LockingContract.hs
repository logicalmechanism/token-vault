{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module LockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
import qualified Plutonomy
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Min Max Data Structures
-------------------------------------------------------------------------------
data VaultOutputDatum =  NoOutputDatum | OutputDatum PlutusV2.Datum
PlutusTx.makeIsDataIndexed ''VaultOutputDatum [('NoOutputDatum, 0), ('OutputDatum, 2)]

data VaultTxOut = VaultTxOut
  { txOutAddress         :: PlutusV2.Address
  , txOutValue           :: PlutusV2.Value
  , txOutDatum           :: VaultOutputDatum
  , txOutReferenceScript :: BuiltinData
  }
PlutusTx.unstableMakeIsData ''VaultTxOut

data VaultTxInInfo = VaultTxInInfo
    { txInInfoOutRef   :: PlutusV2.TxOutRef
    , txInInfoResolved :: VaultTxOut
    } 
PlutusTx.unstableMakeIsData ''VaultTxInInfo

data VaultTxInfo = VaultTxInfo
    { txInfoInputs          :: [VaultTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: BuiltinData
    , txInfoOutputs         :: [VaultTxOut] -- Transaction outputs
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: BuiltinData
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: PlutusV2.POSIXTimeRange -- The valid range for the transaction.
    , txInfoSignatories     :: [PlutusV2.PubKeyHash] -- Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''VaultTxInfo

data VaultScriptPurpose = Spending PlutusV2.TxOutRef
PlutusTx.makeIsDataIndexed ''VaultScriptPurpose [('Spending, 1)]

data VaultScriptContext = VaultScriptContext
  { scriptContextTxInfo :: VaultTxInfo
  , scriptContextPurpose ::  VaultScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''VaultScriptContext
-------------------------------------------------------------------------------
-- | Rebuilt Functions
-------------------------------------------------------------------------------
-- rewrite findOwnInput without higher order functions
{-# inlinable ownInput #-}
ownInput :: VaultScriptContext -> VaultTxOut
ownInput (VaultScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref

-- get the validating script input
{-# inlinable getScriptInput #-}
getScriptInput :: [VaultTxInInfo] -> PlutusV2.TxOutRef -> VaultTxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((VaultTxInInfo tref ot) : tl) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput tl o_ref

-- | Check if a transaction was signed by the given public key.
{-# inlinable signedBy #-}
signedBy :: [PlutusV2.PubKeyHash] -> PlutusV2.PubKeyHash -> Bool
signedBy []     _ = False
signedBy (x:xs) k
  | x == k    = True
  | otherwise =  signedBy xs k

-- | Search a list of TxOut for a TxOut with a specific address that is hodling an exact value.
{-# inlinable isAddrGettingPaidExactly' #-}
isAddrGettingPaidExactly' :: [VaultTxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaidExactly' []     _    _   = False
isAddrGettingPaidExactly' (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaidExactly' xs addr val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = txOutValue x == val
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtPkh       :: PlutusV2.PubKeyHash
  -- ^ A payment public key hash.
  , cdtSc        :: PlutusV2.PubKeyHash
  -- ^ An optional payment staking credential.
  , cdtStartTime :: Integer
  -- ^ The starting lock time in milliseconds.
  , cdtEndTime   :: Integer
  -- ^ The ending lock time in milliseconds.
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> VaultScriptContext -> Bool
mkValidator datum redeemer context = 
  let walletPkh        = cdtPkh datum
      walletAddr       = createAddress walletPkh (cdtSc datum)
      lockTimeInterval = lockBetweenTimeInterval (cdtStartTime datum) (cdtEndTime datum)
      txValidityRange  = txInfoValidRange info
      txSigners        = txInfoSignatories info
  in case redeemer of
    {- | Remove

      A user may remove their staked UTxO if and only if they provide the correct signature,
      receive the validated value, and their validatity range is outside of the time lock interval.
      The contract is designed for many entry UTxOs but only a single script UTxO to be validated
      at a time.
      
    -}
    Remove -> signedBy txSigners walletPkh
           && isAddrGettingPaidExactly' txOutputs walletAddr validatingValue
           && isTxOutsideInterval lockTimeInterval txValidityRange
  where
    info :: VaultTxInfo
    info = scriptContextTxInfo context

    txOutputs :: [VaultTxOut]
    txOutputs = txInfoOutputs info

    validatingValue :: PlutusV2.Value
    validatingValue = txOutValue $ ownInput context
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))

validator :: Validator
validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
-- validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs