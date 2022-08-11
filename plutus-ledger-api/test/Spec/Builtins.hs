-- editorconfig-checker-disable-file
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Spec.Builtins where

import PlutusCore as PLC
import PlutusCore.Data as PLC
import PlutusCore.MkPlc as PLC
import PlutusLedgerApi.Common
import PlutusLedgerApi.Common.Versions
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Scripts
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import UntypedPlutusCore as UPLC

import Codec.Serialise
import Data.ByteString.Lazy as BSL
import Data.ByteString.Short as BSS
import Data.Either
import Data.Foldable (fold, for_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

serialiseDataExScript :: SerialisedScript
serialiseDataExScript = toShort . toStrict $ serialise serialiseDataEx
    where
      serialiseDataEx :: Script
      serialiseDataEx = Script $ UPLC.Program () (PLC.defaultVersion ()) $
                             UPLC.Apply () (UPLC.Builtin () PLC.SerialiseData) (PLC.mkConstant () $ I 1)

errorScript :: SerialisedScript
errorScript = toShort . toStrict $ serialise errorEx
    where
      errorEx :: Script
      errorEx = Script $ UPLC.Program () (PLC.defaultVersion ()) $ UPLC.Error ()

tests :: TestTree
tests =
  testGroup
    "builtins"
    [ testCase "all builtins are available some time" $
            let allPvBuiltins = fold $ Map.elems builtinsIntroducedIn
                allBuiltins = [(toEnum 0)..]
            in for_ allBuiltins $ \f -> assertBool (show f) (f `Set.member` allPvBuiltins)
    , testCase "builtins aren't available before Alonzo" $ assertBool "empty" (Set.null $ builtinsAvailableIn PlutusV1 maryPV) -- l1 valid, p4 invalid
    , testCase "serializeData is only available in l2,Vasil and after" $ do
         assertBool "in l1,Alonzo" $ isLeft $ V1.assertScriptWellFormed alonzoPV serialiseDataExScript
         assertBool "in l1,Vasil" $ isLeft $ V1.assertScriptWellFormed vasilPV serialiseDataExScript
         assertBool "in l2,Alonzo" $ isLeft $ V2.assertScriptWellFormed alonzoPV serialiseDataExScript
         assertBool "in l3,Alonzo" $ isLeft $ V3.assertScriptWellFormed alonzoPV serialiseDataExScript
         assertBool "not in l2,Vasil" $ isRight $ V2.assertScriptWellFormed vasilPV serialiseDataExScript
         assertBool "not in l3,Chang" $ isRight $ V3.assertScriptWellFormed changPV serialiseDataExScript
         assertBool "remdr1" $ isRight $ V1.assertScriptWellFormed changPV $ errorScript <> "remdr1"
         assertBool "remdr2" $ isRight $ V2.assertScriptWellFormed changPV $ errorScript <> "remdr2"
         assertEqual "remdr3" (Left $ RemderError "remdr3") $ V3.assertScriptWellFormed changPV $ errorScript <> "remdr3"
    , testProperty "remdr1gen"$ \remdr -> isRight $ V1.assertScriptWellFormed changPV $ errorScript <> BSS.pack remdr
    , testProperty "remdr2gen"$ \remdr -> isRight $ V2.assertScriptWellFormed changPV $ errorScript <> BSS.pack remdr
    -- we cannot make the same property as above for remdr3gen because it may generate valid bytestring append extensios to the original script
    -- a more sophisticated one could work though
    ]

