-- editorconfig-checker-disable-file
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-pir=0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:max-simplifier-iterations-uplc=0 #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:context-level=0 #-}
{-# OPTIONS_GHC -fmax-simplifier-iterations=0 #-}
{-# OPTIONS_GHC -fno-specialise -O0 #-}
{-# OPTIONS_GHC -fforce-recomp #-}

module Plugin.Basic.Spec where

import Test.Tasty.Extras

import PlutusCore.Test
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code
import PlutusTx.Plugin
import PlutusTx.Prelude as P
import PlutusTx.Test

import Data.Proxy
import PlutusTx.Builtins qualified as Builtin
import Prelude hiding ((+))


basic :: TestNested
basic = testNested "Basic" [
    -- goldenPir "letOverApp" monoId
    goldenPir "letApp" letApp
    , goldenPir "letFunEg2" letFun2
    , goldenPir "letFunInFunAllMulti" letFunInFunAllMulti
    , goldenPir "letFunInFunMulti" letFunInFunMulti
    , goldenPir "letFunInFunMultiFullyApplied" letFunInFunMultiFullyApplied
    , goldenPir "letFunForall" letFunForall
    , goldenPir "letAppMulti" letAppMulti
    , goldenPir "letOverAppMulti" letOverAppMulti
--   , goldenPir "monoK" monoK
--   , goldenPir "letFun" letFun
--   , goldenPir "nonstrictLet" nonstrictLet
--   , goldenPir "strictLet" strictLet
--   , goldenPir "strictMultiLet" strictMultiLet
--   , goldenPir "strictLetRec" strictLetRec
--   -- must keep the scrutinee as it evaluates to error
--   , goldenPir "ifOpt" ifOpt
--   -- should fail
--   , goldenUEval "ifOptEval" [ifOpt]
--   , goldenPir "monadicDo" monadicDo
--   , goldenPir "patternMatchDo" patternMatchDo
--   , goldenUPlcCatch "patternMatchFailure" patternMatchFailure
  ]

letApp :: CompiledCode Integer
{-# NOINLINE letApp #-}
letApp = plc (Proxy @"letApp") (
    let appNum :: Integer
        {-# NOINLINE appNum #-}
        appNum = 4
        funApp :: Integer -> Integer
        {-# NOINLINE funApp #-}
        funApp = (\x y -> Builtin.addInteger x y) appNum
    in funApp 5
    )

letFun2 :: CompiledCode Integer
{-# NOINLINE letFun2 #-}
letFun2 = plc (Proxy @"monoId") (
    let idFun :: Integer -> Integer
        {-# NOINLINE idFun #-}
        idFun = \x -> x
        funApp :: Integer -> (Integer -> Integer)
        {-# NOINLINE funApp #-}
        funApp = \x -> idFun
    in funApp 5 6
    )

letFunInFunAllMulti :: CompiledCode ((Integer -> Integer))
letFunInFunAllMulti = plc (Proxy @"letFunInFunAllMulti") (
    let
        idFun :: Integer -> Integer
        {-# NOINLINE idFun #-}
        idFun x = x
        g :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE g #-}
        g y = idFun
        k :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE k #-}
        k = g
    in k idFun
    )

letFunInFunMulti :: CompiledCode ((Integer -> Integer))
letFunInFunMulti = plc (Proxy @"letFunInFunMulti") (
    let
        idFun :: Integer -> Integer
        {-# NOINLINE idFun #-}
        idFun x = x
        g :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE g #-}
        g y = idFun
    in g idFun
    )

letFunInFunMultiFullyApplied :: CompiledCode Integer
letFunInFunMultiFullyApplied = plc (Proxy @"letFunInFunMultiFullyApplied") (
    let
        idFun :: Integer -> Integer
        {-# NOINLINE idFun #-}
        idFun x = x
        g :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE g #-}
        g y = idFun
    in g idFun 1
    )

letFunForall :: CompiledCode Integer
letFunForall = plc (Proxy @"letFunForall") (
    let
        idFun :: forall a. a -> a
        {-# NOINLINE idFun #-}
        idFun x = x
        g :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE g #-}
        g y = idFun
    in g idFun 1
    )

letAppMulti :: CompiledCode Integer
letAppMulti = plc (Proxy @"letAppMulti")(
    let
        appNum :: Integer
        {-# NOINLINE  appNum #-}
        appNum = 4
        funApp :: Integer -> Integer
        {-# NOINLINE funApp #-}
        funApp = (\x y -> Builtin.addInteger x y) appNum
        k :: Integer -> Integer
        {-# NOINLINE k #-}
        k = funApp
    in k appNum
    )


letOverAppMulti :: CompiledCode Integer
letOverAppMulti = plc (Proxy @"letOverAppMulti")(
    let
        idFun :: Integer -> Integer
        {-# NOINLINE idFun #-}
        idFun y = y
        funApp :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE funApp #-}
        funApp x = idFun
        k :: (Integer -> Integer) -> (Integer -> Integer)
        {-# NOINLINE k #-}
        k = funApp
    in k idFun 6
    )

monoK :: CompiledCode (Integer -> Integer -> Integer)
monoK = plc (Proxy @"monoK") (\(i :: Integer) -> \(_ :: Integer) -> i)

-- GHC actually turns this into a lambda for us, try and make one that stays a let
letFun :: CompiledCode (Integer -> Integer -> Bool)
letFun = plc (Proxy @"letFun") (\(x::Integer) (y::Integer) -> let f z = Builtins.equalsInteger x z in f y)

nonstrictLet :: CompiledCode (Integer -> Integer -> Integer)
nonstrictLet = plc (Proxy @"strictLet") (\(x::Integer) (y::Integer) -> let z = Builtins.addInteger x y in Builtins.addInteger z z)

-- GHC turns strict let-bindings into case expressions, which we correctly turn into strict let-bindings
strictLet :: CompiledCode (Integer -> Integer -> Integer)
strictLet = plc (Proxy @"strictLet") (\(x::Integer) (y::Integer) -> let !z = Builtins.addInteger x y in Builtins.addInteger z z)

strictMultiLet :: CompiledCode (Integer -> Integer -> Integer)
strictMultiLet = plc (Proxy @"strictLet") (\(x::Integer) (y::Integer) -> let !z = Builtins.addInteger x y; !q = Builtins.addInteger z z; in Builtins.addInteger q q)

-- Here we see the wrinkles of GHC's codegen: GHC creates let-bindings for the recursion, with _nested_ case expressions for the strictness.
-- So we get non-strict external bindings for z and q, and inside that we get strict bindings corresponding to the case expressions.
strictLetRec :: CompiledCode (Integer -> Integer -> Integer)
strictLetRec = plc (Proxy @"strictLetRec") (\(x::Integer) (y::Integer) -> let !z = Builtins.addInteger x q; !q = Builtins.addInteger y z in Builtins.addInteger z z)

ifOpt :: CompiledCode Integer
ifOpt = plc (Proxy @"ifOpt") (if ((1 `Builtins.divideInteger` 0) `Builtins.equalsInteger` 0) then 1 else 1)

-- TODO: It's pretty questionable that this works at all! It's actually using 'Monad' from 'base',
-- since that's what 'do' notation is hard-coded to use, and it just happens that it's all inlinable
-- enough to work...
-- Test what basic do-notation looks like (should just be a bunch of calls to '>>=')
monadicDo :: CompiledCode (Maybe Integer -> Maybe Integer -> Maybe Integer)
monadicDo = plc (Proxy @"monadicDo") (\(x :: Maybe Integer) (y:: Maybe Integer) -> do
    x' <- x
    y' <- y
    P.pure (x' `Builtins.addInteger` y'))

-- Irrefutable match in a do block
patternMatchDo :: CompiledCode (Maybe (Integer, Integer) -> Maybe Integer -> Maybe Integer)
patternMatchDo = plc (Proxy @"patternMatchDo") (\(x :: Maybe (Integer, Integer)) (y:: Maybe Integer) -> do
    (x1, x2) <- x
    y' <- y
    P.pure (x1 `Builtins.addInteger` x2 `Builtins.addInteger` y'))

-- Should fail, since it'll call 'MonadFail.fail' with a String, which won't work
patternMatchFailure :: CompiledCode (Maybe (Maybe Integer) -> Maybe Integer -> Maybe Integer)
patternMatchFailure = plc (Proxy @"patternMatchFailure") (\(x :: Maybe (Maybe Integer)) (y:: Maybe Integer) -> do
    Just x' <- x
    y' <- y
    P.pure (x' `Builtins.addInteger` y'))
