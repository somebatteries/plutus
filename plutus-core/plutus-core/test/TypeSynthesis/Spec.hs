{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module TypeSynthesis.Spec
    ( test_typecheck
    ) where

import PlutusPrelude

import PlutusCore
import PlutusCore.Builtin
import PlutusCore.Default
import PlutusCore.FsTree
import PlutusCore.Pretty

import PlutusCore.Examples.Builtins
import PlutusCore.Examples.Everything (builtins, examples)
import PlutusCore.StdLib.Everything (stdLib)

import Control.Monad.Except
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Extras
import Test.Tasty.HUnit

kindcheck
    :: (uni ~ DefaultUni, fun ~ DefaultFun, MonadError (Error uni fun ()) m)
    => Type TyName uni () -> m (Type TyName uni ())
kindcheck ty = do
    _ <- runQuoteT $ inferKind ty
    return ty

typecheck
    :: (uni ~ DefaultUni, MonadError (Error uni fun ()) m, ToBuiltinMeaning uni fun)
    => BuiltinVersion fun -> Term TyName Name uni fun () -> m ()
typecheck ver term = do
    _ <- runQuoteT $ do
        tcConfig <- TypeCheckConfig <$> builtinMeaningsToTypes ver ()
        inferType tcConfig term
    return ()

-- | Assert a 'Type' is well-kinded.
assertWellKinded :: HasCallStack => Type TyName DefaultUni () -> Assertion
assertWellKinded ty = case runExcept . runQuoteT $ kindcheck ty of
    Left  err -> assertFailure $ "Kind error: " ++ displayPlcCondensedErrorClassic err
    Right _   -> return ()

-- | Assert a 'Term' is well-typed.
assertWellTyped
    :: (HasCallStack, ToBuiltinMeaning DefaultUni fun, Pretty fun)
    => BuiltinVersion fun
    -> Term TyName Name DefaultUni fun () -> Assertion
assertWellTyped ver term = case runExcept . runQuoteT $ typecheck ver term of
    Left  err -> assertFailure $ "Type error: " ++ displayPlcCondensedErrorClassic err
    Right _   -> return ()

-- | Assert a term is ill-typed.
assertIllTyped :: HasCallStack => BuiltinVersion DefaultFun -> Term TyName Name DefaultUni DefaultFun () -> Assertion
assertIllTyped ver term = case runExcept . runQuoteT $ typecheck ver term of
    Right () -> assertFailure $ "Well-typed: " ++ displayPlcCondensedErrorClassic term
    Left  _  -> return ()

foldAssertWell
    :: (ToBuiltinMeaning DefaultUni fun, Pretty fun)
    => BuiltinVersion fun
    -> PlcFolderContents DefaultUni fun -> [TestTree]
foldAssertWell ver =
    foldPlcFolderContents
        testGroup
        (\name -> testCase name . assertWellKinded)
        (\name -> testCase name . assertWellTyped ver)

test_typecheckAvailable :: TestTree
test_typecheckAvailable =
    testGroup "Available"
        [ testGroup "DefaultFun"   $ foldAssertWell currentVerDefaultFun stdLib
        , testGroup "ExtensionFun" $ foldAssertWell ExtensionFunV1 builtins
        , testGroup "Both"         $ foldAssertWell (EitherV currentVerDefaultFun ExtensionFunV1) examples
        ]

-- | Self-application. An example of ill-typed term.
--
-- > /\ (A :: *) -> \(x : A) -> x x
selfApply :: Term TyName Name uni fun ()
selfApply = runQuote $ do
    a <- freshTyName "a"
    x <- freshName "x"
    return
        . TyAbs () a (Type ())
        . LamAbs () x (TyVar () a)
        . Apply () (Var () x)
        $ Var () x

test_typecheckIllTyped :: TestTree
test_typecheckIllTyped =
    testCase "ill-typed" $
        foldMap (assertIllTyped currentVerDefaultFun)
            [ selfApply
            ]

test_typecheckFun :: (ToBuiltinMeaning DefaultUni fun, Show fun) => BuiltinVersion fun -> fun -> TestTree
test_typecheckFun ver  name = goldenVsDoc testName path doc where
    testName = show name
    path     = "plutus-core" </> "test" </> "TypeSynthesis" </> "Golden" </> (testName ++ ".plc.golden")
    doc      = prettyPlcDef $ typeOfBuiltinFunction @DefaultUni ver name

test_typecheckAllFun
    :: forall fun. (ToBuiltinMeaning DefaultUni fun, Show fun)
    => BuiltinVersion fun
    -> String -> TestTree
test_typecheckAllFun ver name = testGroup name . map (test_typecheckFun ver) $ enumerate @fun

test_typecheckDefaultFuns :: TestTree
test_typecheckDefaultFuns =
    testGroup "builtins"
        [ test_typecheckAllFun @DefaultFun currentVerDefaultFun "DefaultFun"
        , test_typecheckAllFun @ExtensionFun ExtensionFunV1 "ExtensionFun"
        ]

test_typecheck :: TestTree
test_typecheck =
    testGroup "typecheck"
        [ test_typecheckDefaultFuns
        , test_typecheckAvailable
        , test_typecheckIllTyped
        ]
