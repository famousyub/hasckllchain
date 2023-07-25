{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Gen.Bcc.Api.Typed
  ( genAddressCole
  , genAddressSophie
  , genCertificate
  , genMaybeOptimumNonce
  , genOptimumNonce
  , genProtocolParameters
  , genValueNestedRep
  , genValueNestedBundle
  , genColeKeyWitness

  , genTxId
  , genTxIn
  , genTxOut
  , genUTxO

    -- * Scripts
  , genScript
  , genSimpleScript
  , genZerepochScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , genScriptData

  , genOperationalCertificate
  , genOperationalCertificateIssueCounter
  , genSophieWitness
  , genSigningKey
  , genStakeAddress
  , genTx
  , genTxBody
  , genValue
  , genValueDefault
  , genVerificationKey
  , genVerificationKeyHash
  , genUpdateProposal
  , genProtocolParametersUpdate
  , genScriptDataSupportedInAurumEra
  , genTxOutDatumHash
  , genTxOutValue
  , genValueForTxOut
  , genValueForMinting

  , genRational
  ) where

import           Bcc.Api hiding (txIns)
import qualified Bcc.Api as Api
import           Bcc.Api.Cole (KeyWitness (ColeKeyWitness), Entropic (Entropic),
                   WitnessNetworkIdOrColeAddress (..))
import           Bcc.Api.Sophie (Hash (ScriptDataHash), KESPeriod (KESPeriod),
                   OperationalCertificateIssueCounter (OperationalCertificateIssueCounter),
                   ZerepochScript (ZerepochScriptSerialised), ProtocolParameters (ProtocolParameters),
                   StakeCredential (StakeCredentialByKey), StakePoolKey)

import           Bcc.Prelude

import           Control.Monad.Fail (fail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import           Data.Coerce
import           Data.String

import qualified Bcc.Binary as CBOR
import qualified Bcc.Crypto.Hash as Crypto
import qualified Bcc.Crypto.Seed as Crypto
import qualified Zerepoch.V1.Ledger.Api as Zerepoch
import qualified Sophie.Spec.Ledger.TxBody as Ledger (EraIndependentTxBody)

import           Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Bcc.Crypto.Hash.Class as CRYPTO
import           Bcc.Ledger.SafeHash (unsafeMakeSafeHash)
import           Gen.Bcc.Api.Metadata (genTxMetadata)
import           Test.Bcc.Chain.UTxO.Gen (genVKWitness)
import           Test.Bcc.Crypto.Gen (genProtocolMagicId)

{- HLINT ignore "Reduce duplication" -}

genAddressCole :: Gen (Address ColeAddr)
genAddressCole = makeColeAddress <$> genNetworkId
                                   <*> genVerificationKey AsColeKey

genAddressSophie :: Gen (Address SophieAddr)
genAddressSophie = makeSophieAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: BccEra era -> Gen (AddressInEra era)
genAddressInEra era =
  case bccEraStyle era of
    LegacyColeEra ->
      coleAddressInEra <$> genAddressCole

    SophieBasedEra _ ->
      Gen.choice
        [ coleAddressInEra   <$> genAddressCole
        , sophieAddressInEra <$> genAddressSophie
        ]

genKESPeriod :: Gen KESPeriod
genKESPeriod = KESPeriod <$> Gen.word Range.constantBounded

genEntropic :: Gen Entropic
genEntropic = Entropic <$> Gen.integral (Range.linear 0 5000)


----------------------------------------------------------------------------
-- SimpleScript generators
--

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang
genScript (ZerepochScriptLanguage lang) =
    ZerepochScript lang <$> genZerepochScript lang

genSimpleScript :: SimpleScriptVersion lang -> Gen (SimpleScript lang)
genSimpleScript lang =
    genTerm
  where
    genTerm = Gen.recursive Gen.choice nonRecursive recursive

    -- Non-recursive generators
    nonRecursive =
         (RequireSignature . verificationKeyHash <$>
             genVerificationKey AsPaymentKey)

      : [ RequireTimeBefore supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

     ++ [ RequireTimeAfter supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

    -- Recursive generators
    recursive =
      [ RequireAllOf <$> Gen.list (Range.linear 0 10) genTerm

      , RequireAnyOf <$> Gen.list (Range.linear 0 10) genTerm

      , do ts <- Gen.list (Range.linear 0 10) genTerm
           m  <- Gen.integral (Range.constant 0 (length ts))
           return (RequireMOf m ts)
      ]

genZerepochScript :: ZerepochScriptVersion lang -> Gen (ZerepochScript lang)
genZerepochScript _ =
    -- We make no attempt to create a valid script
    ZerepochScriptSerialised . SBS.toShort <$> Gen.bytes (Range.linear 0 32)

genScriptData :: Gen ScriptData
genScriptData =
    Gen.recursive
      Gen.choice
        [ ScriptDataNumber <$> genInteger
        , ScriptDataBytes  <$> genByteString
        ]
        -- The Gen.recursive combinator calls these with the size halved
        [ ScriptDataConstructor <$> genInteger
                                <*> genScriptDataList
        , ScriptDataList <$> genScriptDataList
        , ScriptDataMap  <$> genScriptDataMap
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    0
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

    genScriptDataList :: Gen [ScriptData]
    genScriptDataList =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) genScriptData

    genScriptDataMap  :: Gen [(ScriptData, ScriptData)]
    genScriptDataMap =
      Gen.sized $ \sz ->
        Gen.list (Range.linear 0 (fromIntegral sz)) $
          (,) <$> genScriptData <*> genScriptData


-- ----------------------------------------------------------------------------
-- Script generators for any language, or any language valid in a specific era
--

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
    Gen.choice
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: BccEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    Gen.choice
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)


----------------------------------------------------------------------------
-- Multi-asset generators
--

genAssetName :: Gen AssetName
genAssetName =
  Gen.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element ["", "a", "b", "c"])
    , (1, AssetName <$> Gen.utf8 (Range.singleton  32) Gen.alphaNum)
    , (1, AssetName <$> Gen.utf8 (Range.constant 1 31) Gen.alphaNum)
    ]

genPolicyId :: Gen PolicyId
genPolicyId =
  Gen.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, Gen.element [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId = Gen.choice [ AssetId <$> genPolicyId <*> genAssetName
                        , return BccAssetId
                        ]

genQuantity :: Range Integer -> Gen Quantity
genQuantity range = fromInteger <$> Gen.integral range

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity (Range.constantFrom 0 (-2) 2)

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = genQuantity (Range.constant 0 2)

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    Gen.list (Range.constant 0 10)
             ((,) <$> genAId <*> genQuant)

-- | Generate a 'Value' with any asset ID and a positive or negative quantity.
genValueDefault :: Gen Value
genValueDefault = genValue genAssetId genSignedQuantity

-- | Generate a 'Value' suitable for minting, i.e. non-BCC asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoBcc genSignedQuantity
  where
    genAssetIdNoBcc :: Gen AssetId
    genAssetIdNoBcc = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity


-- Note that we expect to sometimes generate duplicate policy id keys since we
-- pick 90% of policy ids from a set of just three.
genValueNestedRep :: Gen ValueNestedRep
genValueNestedRep =
  ValueNestedRep <$> Gen.list (Range.constant 0 5) genValueNestedBundle

genValueNestedBundle :: Gen ValueNestedBundle
genValueNestedBundle =
  Gen.choice
    [ ValueNestedBundleBcc <$> genSignedQuantity
    , ValueNestedBundle <$> genPolicyId
                        <*> Gen.map (Range.constant 0 5)
                                    ((,) <$> genAssetName <*> genSignedQuantity)
    ]

genNetworkId :: Gen NetworkId
genNetworkId =
  Gen.choice
    [ pure Mainnet
    , Testnet <$> genNetworkMagic
    ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = NetworkMagic <$> Gen.word32 Range.constantBounded

genOperationalCertificate :: Gen OperationalCertificate
genOperationalCertificate = fst <$> genOperationalCertificateWithCounter

genOperationalCertificateIssueCounter :: Gen OperationalCertificateIssueCounter
genOperationalCertificateIssueCounter = snd <$> genOperationalCertificateWithCounter

genOperationalCertificateWithCounter :: Gen (OperationalCertificate, OperationalCertificateIssueCounter)
genOperationalCertificateWithCounter = do
    kesVKey <- genVerificationKey AsKesKey
    stkPoolOrGenDelExtSign <- Gen.either (genSigningKey AsStakePoolKey) (genSigningKey AsGenesisDelegateExtendedKey)
    kesP <- genKESPeriod
    c <- Gen.integral $ Range.linear 0 1000
    let stakePoolVer = either getVerificationKey (convert . getVerificationKey) stkPoolOrGenDelExtSign
        iCounter = OperationalCertificateIssueCounter c stakePoolVer

    case issueOperationalCertificate kesVKey stkPoolOrGenDelExtSign kesP iCounter of
      -- This case should be impossible as we clearly derive the verification
      -- key from the generated signing key.
      Left err -> fail $ displayError err
      Right pair -> return pair
  where
    convert :: VerificationKey GenesisDelegateExtendedKey
            -> VerificationKey StakePoolKey
    convert = (castVerificationKey :: VerificationKey GenesisDelegateKey
                                   -> VerificationKey StakePoolKey)
            . (castVerificationKey :: VerificationKey GenesisDelegateExtendedKey
                                   -> VerificationKey GenesisDelegateKey)


-- TODO: Generate payment credential via script
genPaymentCredential :: Gen PaymentCredential
genPaymentCredential = do
  vKey <- genVerificationKey AsPaymentKey
  return . PaymentCredentialByKey $ verificationKeyHash vKey

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddress :: Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

-- TODO: Generate StakeAddressReference via pointer
genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  Gen.choice
    [ StakeAddressByValue <$> genStakeCredential
    , return NoStakeAddress
    ]

-- TODO: Generate StakeCredential via script
genStakeCredential :: Gen StakeCredential
genStakeCredential = do
  vKey <- genVerificationKey AsStakeKey
  return . StakeCredentialByKey $ verificationKeyHash vKey

genSophieHash :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genSophieHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> Gen.word64 Range.constantBounded

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genSophieHash

genTxIndex :: Gen TxIx
genTxIndex = TxIx <$> Gen.word Range.constantBounded

genTxOutValue :: BccEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case multiAssetSupportedInEra era of
    Left bccOnlyInEra     -> TxOutBccOnly bccOnlyInEra <$> genEntropic
    Right multiAssetInEra -> TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOut :: BccEra era -> Gen (TxOut era)
genTxOut era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHash era

genUTxO :: BccEra era -> Gen (UTxO era)
genUTxO era =
  UTxO <$> Gen.map (Range.constant 0 5) ((,) <$> genTxIn <*> genTxOut era)

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: BccEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: BccEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      panic "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: BccEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: BccEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
      Gen.choice
        [ pure TxMetadataNone
        , TxMetadataInEra supported <$> genTxMetadata
        ]

genTxAuxScripts :: BccEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        Gen.list (Range.linear 0 3)
                 (genScriptInEra era)

genTxWithdrawals :: BccEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing -> pure TxWithdrawalsNone
    Just supported ->
      Gen.choice
        [ pure TxWithdrawalsNone
        , pure (TxWithdrawals supported mempty)
          -- TODO: Generate withdrawals
        ]

genTxCertificates :: BccEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
  case certificatesSupportedInEra era of
    Nothing -> pure TxCertificatesNone
    Just supported -> do
      certs <- Gen.list (Range.constant 0 3) genCertificate
      Gen.choice
        [ pure TxCertificatesNone
        , pure (TxCertificates supported certs $ BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

-- TODO: Add remaining certificates
genCertificate :: Gen Certificate
genCertificate =
  Gen.choice
    [ StakeAddressRegistrationCertificate <$> genStakeCredential
    , StakeAddressDeregistrationCertificate <$> genStakeCredential
    ]
    
genTxUpdateProposal :: BccEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case updateProposalSupportedInEra era of
    Nothing -> pure TxUpdateProposalNone
    Just supported ->
      Gen.choice
        [ pure TxUpdateProposalNone
        , TxUpdateProposal supported <$> genUpdateProposal
        ]

genTxMintValue :: BccEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      Gen.choice
        [ pure TxMintNone
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]

genTxBodyContent :: BccEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
  txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) genTxIn
  txInsCollateral <- genTxInsCollateral era
  txOuts <- Gen.list (Range.constant 1 10) (genTxOut era)
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraScriptData = BuildTxWith TxExtraScriptDataNone --TODO: Aurum era: Generate extra script data
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Aurum era: Generate witness key hashes
  txProtocolParams <- BuildTxWith <$> Gen.maybe genProtocolParameters
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txOuts
    , Api.txFee
    , Api.txValidityRange
    , Api.txMetadata
    , Api.txAuxScripts
    , Api.txExtraScriptData
    , Api.txExtraKeyWits
    , Api.txProtocolParams
    , Api.txWithdrawals
    , Api.txCertificates
    , Api.txUpdateProposal
    , Api.txMintValue
    , Api.txScriptValidity
    }

genTxInsCollateral :: BccEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> Gen.choice
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> Gen.list (Range.linear 0 10) genTxIn
                          ]

genTxFee :: BccEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> genEntropic

genTxBody :: IsBccEra era => BccEra era -> Gen (TxBody era)
genTxBody era = do
  res <- makeTransactionBody <$> genTxBodyContent era
  case res of
    Left err -> fail (displayError err)
    Right txBody -> pure txBody

genTxScriptValidity :: BccEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInBccEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = Gen.element [ScriptInvalid, ScriptValid]

genTx :: forall era. IsBccEra era => BccEra era -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> genTxBody era

genWitnesses :: BccEra era -> Gen [KeyWitness era]
genWitnesses era =
  case bccEraStyle era of
    LegacyColeEra    -> Gen.list (Range.constant 1 10) genColeKeyWitness
    SophieBasedEra _ -> do
      bsWits  <- Gen.list (Range.constant 0 10)
                          (genSophieBootstrapWitness era)
      keyWits <- Gen.list (Range.constant 0 10)
                          (genSophieKeyWitness era)
      return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genColeKeyWitness :: Gen (KeyWitness ColeEra)
genColeKeyWitness = do
  pmId <- genProtocolMagicId
  txinWitness <- genVKWitness pmId
  return $ ColeKeyWitness txinWitness

genWitnessNetworkIdOrColeAddress :: Gen WitnessNetworkIdOrColeAddress
genWitnessNetworkIdOrColeAddress =
  Gen.choice
    [ WitnessNetworkId <$> genNetworkId
    , WitnessColeAddress <$> genAddressCole
    ]

genSophieBootstrapWitness
  :: IsSophieBasedEra era
  => BccEra era
  -> Gen (KeyWitness era)
genSophieBootstrapWitness era =
 makeSophieBootstrapWitness
   <$> genWitnessNetworkIdOrColeAddress
   <*> genTxBody era
   <*> genSigningKey AsColeKey

genSophieKeyWitness
  :: IsSophieBasedEra era
  => BccEra era
  -> Gen (KeyWitness era)
genSophieKeyWitness era =
  makeSophieKeyWitness
    <$> genTxBody era
    <*> genSophieWitnessSigningKey

genSophieWitness
  :: IsSophieBasedEra era
  => BccEra era
  -> Gen (KeyWitness era)
genSophieWitness era =
  Gen.choice
   [ genSophieKeyWitness era
   , genSophieBootstrapWitness era
   ]

genSophieWitnessSigningKey :: Gen SophieWitnessSigningKey
genSophieWitnessSigningKey =
  Gen.choice [ WitnessPaymentKey <$>  genSigningKey AsPaymentKey
             , WitnessPaymentExtendedKey <$>  genSigningKey AsPaymentExtendedKey
             , WitnessStakeKey <$>  genSigningKey AsStakeKey
             , WitnessStakePoolKey <$>  genSigningKey AsStakePoolKey
             , WitnessGenesisDelegateKey <$>  genSigningKey AsGenesisDelegateKey
             , WitnessVestedDelegateKey <$>  genSigningKey AsVestedDelegateKey
             , WitnessGenesisUTxOKey <$>  genSigningKey AsGenesisUTxOKey
             ]

genSeed :: Int -> Gen Crypto.Seed
genSeed n = Crypto.mkSeedFromBytes <$> Gen.bytes (Range.singleton n)

genNat :: Gen Natural
genNat = Gen.integral (Range.linear 0 10)

genRational :: Gen Rational
genRational =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Word64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Word64 -> Rational
    ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/The-Blockchain-Company/bcc-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = Gen.integral (Range.linear 1 maxBound)

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> Gen.word64 (Range.linear 0 10)

genOptimumNonce :: Gen OptimumNonce
genOptimumNonce = makeOptimumNonce <$> Gen.bytes (Range.linear 0 32)

genMaybeOptimumNonce :: Gen (Maybe OptimumNonce)
genMaybeOptimumNonce = Gen.maybe genOptimumNonce

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> genRational
    <*> genMaybeOptimumNonce
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> Gen.maybe genEntropic
    <*> genEntropic
    <*> genEntropic
    <*> genEntropic
    <*> genEpochNo
    <*> genNat
    <*> genRationalInt64
    <*> genRational
    <*> genRational
    <*> Gen.maybe genEntropic
    <*> genCostModels
    <*> Gen.maybe genExecutionUnitPrices
    <*> Gen.maybe genExecutionUnits
    <*> Gen.maybe genExecutionUnits
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> Gen.maybe ((,) <$> genNat <*> genNat)
    <*> Gen.maybe genRational
    <*> Gen.maybe genMaybeOptimumNonce
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genEntropic
    <*> Gen.maybe genEntropic
    <*> Gen.maybe genEntropic
    <*> Gen.maybe genEntropic
    <*> Gen.maybe genEpochNo
    <*> Gen.maybe genNat
    <*> Gen.maybe genRationalInt64
    <*> Gen.maybe genRational
    <*> Gen.maybe genRational
    <*> Gen.maybe genEntropic
    <*> genCostModels
    <*> Gen.maybe genExecutionUnitPrices
    <*> Gen.maybe genExecutionUnits
    <*> Gen.maybe genExecutionUnits
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat
    <*> Gen.maybe genNat


genUpdateProposal :: Gen UpdateProposal
genUpdateProposal =
  UpdateProposal
    <$> Gen.map (Range.constant 1 3)
                ((,) <$> genVerificationKeyHash AsGenesisKey
                     <*> genProtocolParametersUpdate)
    <*> genEpochNo

genCostModel :: Gen CostModel
genCostModel = case Zerepoch.defaultCostModelParams of
  Nothing -> panic "Zerepoch defaultCostModelParams is broken."
  Just dcm ->
      CostModel
    -- TODO This needs to be the cost model struct for whichever
    -- Zerepoch version we're using, once we support multiple Zerepoch versions.
    <$> mapM (const $ Gen.integral (Range.linear 0 5000)) dcm

genCostModels :: Gen (Map AnyZerepochScriptVersion CostModel)
genCostModels =
    Gen.map (Range.linear 0 (length zerepochScriptVersions))
            ((,) <$> Gen.element zerepochScriptVersions
                 <*> genCostModel)
  where
    zerepochScriptVersions :: [AnyZerepochScriptVersion]
    zerepochScriptVersions = [minBound..maxBound]

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                   <*> Gen.integral (Range.constant 0 1000)

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHash :: BccEra era -> Gen (TxOutDatumHash era)
genTxOutDatumHash era = case era of
    ColeEra -> pure TxOutDatumHashNone
    SophieEra -> pure TxOutDatumHashNone
    EvieEra -> pure TxOutDatumHashNone
    JenEra -> pure TxOutDatumHashNone
    AurumEra -> Gen.choice
      [ pure TxOutDatumHashNone
      , TxOutDatumHash ScriptDataInAurumEra <$> genHashScriptData
      ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Bcc.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

genScriptDataSupportedInAurumEra :: Gen (ScriptDataSupportedInEra AurumEra)
genScriptDataSupportedInAurumEra = pure ScriptDataInAurumEra
