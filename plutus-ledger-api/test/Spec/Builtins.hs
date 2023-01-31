{-# LANGUAGE TypeApplications #-}
module Spec.Builtins where

import Plutus.V1.Ledger.Api qualified as V1
import Plutus.V1.Ledger.Scripts
import Plutus.V2.Ledger.Api qualified as V2
import PlutusCore as PLC
import PlutusCore.Data as PLC
import PlutusCore.MkPlc as PLC
import UntypedPlutusCore as UPLC

import Codec.Serialise
import Data.ByteString.Lazy as BSL
import Data.ByteString.Short
import Data.Foldable (fold, for_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Plutus.ApiCommon as Common
import Plutus.V1.Ledger.ProtocolVersions
import Test.Tasty
import Test.Tasty.HUnit

serialiseDataExScript :: SerializedScript
serialiseDataExScript = toShort . toStrict $ serialise serialiseDataEx
    where
      serialiseDataEx :: Script
      serialiseDataEx = Script (UPLC.Program () (PLC.defaultVersion ()) $
                             UPLC.Apply () (UPLC.Builtin () PLC.SerialiseData) (PLC.mkConstant () $ I 1)) defaultPIRprog

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
         assertBool "in l1,Alonzo" $ not $ V1.isScriptWellFormed alonzoPV serialiseDataExScript
         assertBool "in l1,Vasil" $ not $ V1.isScriptWellFormed vasilPV serialiseDataExScript
         assertBool "in l2,Alonzo" $ not $ V2.isScriptWellFormed alonzoPV serialiseDataExScript
         assertBool "not in l2,Vasil" $ V2.isScriptWellFormed vasilPV serialiseDataExScript
    , testCase "cost model parameters" $
         -- v1 is missing some cost model parameters because new builtins are added in v2
         assertBool "v1 params is proper subset of v2 params" $ V1.costModelParamNames `Set.isProperSubsetOf` V2.costModelParamNames
    ]
