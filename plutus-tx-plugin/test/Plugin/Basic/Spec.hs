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
module Plugin.Basic.Spec where

import Test.Tasty.Extras

import PlutusCore.Test
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Code
import PlutusTx.Plugin
import PlutusTx.Prelude as P
import PlutusTx.Test

import Data.Proxy


basic :: TestNested
basic = testNested "Basic" [
    goldenPir "letFunConst" monoId
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

monoId :: CompiledCode Integer
{-# NOINLINE monoId #-}
monoId = plc (Proxy @"monoId") (
    let constFun :: Integer -> Bool -> Integer
        {-# NOINLINE constFun #-}
        constFun x y = x
    in constFun 3 False
    )

eg1 :: CompiledCode (Integer -> Integer)
eg1 = plc (Proxy @"eg1") (
    let id :: Integer -> Integer
        id x = x
        constantId :: Integer -> (Integer -> Integer)
        constantId y = id
    in constantId 10
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
