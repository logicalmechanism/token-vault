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
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
import qualified Plutonomy

{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Min Max Data Structures
-------------------------------------------------------------------------------
data VaultTxOut = VaultTxOut
  { txOutAddress         :: PlutusV2.Address
  , txOutValue           :: PlutusV2.Value
  , txOutDatum           :: PlutusV2.OutputDatum
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

data VaultScriptContext = VaultScriptContext
  { scriptContextTxInfo :: VaultTxInfo
  , scriptContextPurpose ::  PlutusV2.ScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''VaultScriptContext

-- | Find the input currently being validated.
findOwnInput' :: VaultScriptContext -> Maybe VaultTxInInfo
findOwnInput' VaultScriptContext{scriptContextTxInfo=VaultTxInfo{txInfoInputs}, scriptContextPurpose=PlutusV2.Spending txOutRef} =
    find (\VaultTxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef) txInfoInputs
findOwnInput' _ = Nothing

-- | Get all the outputs that pay to the same script address we are currently spending from, if any.
getContinuingOutputs' :: VaultScriptContext -> [VaultTxOut]
getContinuingOutputs' ctx | Just VaultTxInInfo{txInInfoResolved=VaultTxOut{txOutAddress}} <- findOwnInput' ctx = filter (f txOutAddress) (txInfoOutputs $ scriptContextTxInfo ctx)
    where
        f addr VaultTxOut{txOutAddress=otherAddress} = addr == otherAddress
getContinuingOutputs' _ = traceError "Lf" -- "Can't get any continuing outputs"

-- | Check if a transaction was signed by the given public key.
txSignedBy' :: VaultTxInfo -> PlutusV2.PubKeyHash -> Bool
txSignedBy' VaultTxInfo{txInfoSignatories} k = case find ((==) k) txInfoSignatories of
    Just _  -> True
    Nothing -> False

-- | Count the number of inputs that have datums of any kind.
isNInputs' :: [VaultTxInInfo] -> Integer -> Bool
isNInputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [VaultTxInInfo] -> Integer -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case txOutDatum $ txInInfoResolved x of
        PlutusV2.NoOutputDatum         -> loopInputs xs   counter
        ( PlutusV2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( PlutusV2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline

-- | Count the number of outputs that have datums of any kind.
isNOutputs' :: [VaultTxOut] -> Integer -> Bool
isNOutputs' utxos number = loopInputs utxos 0
  where
    loopInputs :: [VaultTxOut] -> Integer  -> Bool
    loopInputs []     counter = counter == number
    loopInputs (x:xs) counter = 
      case txOutDatum x of
        PlutusV2.NoOutputDatum         -> loopInputs xs   counter
        ( PlutusV2.OutputDatumHash _ ) -> loopInputs xs ( counter + 1 ) -- embedded
        ( PlutusV2.OutputDatum     _ ) -> loopInputs xs ( counter + 1 ) -- inline

-- | Search a list of TxOut for a TxOut with a specific address that is hodling an exact amount of of a singular token. 
isAddrGettingPaidExactly' :: [VaultTxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
isAddrGettingPaidExactly' []     _    _   = False
isAddrGettingPaidExactly' (x:xs) addr val
  | checkAddr && checkVal = True
  | otherwise             = isAddrGettingPaidExactly' xs addr val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == addr

    checkVal :: Bool
    checkVal = txOutValue x == val     -- must be exact
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
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove, 0 ) ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> VaultScriptContext -> Bool
mkValidator datum redeemer context =
  case redeemer of
    {- | Remove

      A user may remove their staked UTxO if and only if they provide the correct signature,
      receive the validated value, and their validatity range is outside of the time lock interval.
      The contract is designed for many entry UTxOs but only a single script UTxO to be validated
      at a time.
      
    -}
    Remove -> do
      { let walletPkh        = cdtPkh datum
      ; let walletAddr       = createAddress walletPkh (cdtSc datum)
      ; let lockTimeInterval = lockBetweenTimeInterval (cdtStartTime datum) (cdtEndTime datum)
      ; let txValidityRange  = txInfoValidRange info
      ; let a = traceIfFalse "Tx Signer"    $ txSignedBy' info walletPkh                          -- wallet must sign it
      ; let b = traceIfFalse "Bad In/Out"   $ isNInputs' txInputs 1 && isNOutputs' contTxOutputs 0            -- single input, no cont output
      ; let c = traceIfFalse "Return Value" $ isAddrGettingPaidExactly' txOutputs walletAddr validatingValue -- wallet must get the UTxO
      ; let d = traceIfFalse "Time Locking" $ isTxOutsideInterval lockTimeInterval txValidityRange          -- UTxO is not time locked
      ;         traceIfFalse "Remove Error" $ all (==(True :: Bool)) [a,b,c,d]
      }
  where
    info :: VaultTxInfo
    info = scriptContextTxInfo context

    txInputs :: [VaultTxInInfo]
    txInputs = txInfoInputs info

    txOutputs :: [VaultTxOut]
    txOutputs = txInfoOutputs info

    contTxOutputs :: [VaultTxOut]
    contTxOutputs = getContinuingOutputs' context

    validatingValue :: PlutusV2.Value
    validatingValue =
      case findOwnInput' context of
        Nothing    -> traceError "No Input to Validate."
        Just input -> txOutValue $ txInInfoResolved input
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------

wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrappedValidator = wrap mkValidator
wrappedValidator x y z = check (mkValidator (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))


validator :: Validator
validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

-- validator' :: PlutusV2.Validator
-- validator' = PlutusV2.mkValidatorScript
--     $$(PlutusTx.compile [|| wrap ||])
--  where
--     wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
-- script :: Scripts.Script
-- script = Scripts.unValidatorScript validator'

lockingContractScriptShortBs :: SBS.ShortByteString
-- lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

lockingContractScript :: PlutusScript PlutusScriptV2
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs