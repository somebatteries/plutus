{-# LANGUAGE DeriveAnyClass #-}

module UntypedPlutusCore (
    module Export
    , Term (..)
    , Program (..)
    , applyProgram
    , parseScoped
    , PLC.DefaultUni
    , PLC.DefaultFun
    , mkDefaultProg
    , UPLCError
    ) where

import UntypedPlutusCore.Check.Scope as Export
import UntypedPlutusCore.Core as Export
import UntypedPlutusCore.DeBruijn as Export
import UntypedPlutusCore.Parser as Parser (parseScoped)
import UntypedPlutusCore.Simplify as Export
import UntypedPlutusCore.Size as Export
import UntypedPlutusCore.Subst as Export

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import PlutusCore.Core qualified as PLC
import PlutusCore.Default qualified as PLC
import PlutusCore.Error
import PlutusCore.Name as Export

-- | Take one UPLC program and apply it to another.
applyProgram :: Program name uni fun () -> Program name uni fun () -> Program name uni fun ()
applyProgram (Program _ _ t1) (Program _ _ t2) = Program () (PLC.defaultVersion ()) (Apply () t1 t2)

{- | DON'T USE, we'll be getting rid of `defaultVersion`.
Turn a UPLC term to a UPLC program with the default version. -}
mkDefaultProg :: Term name uni fun () -> Program name uni fun ()
mkDefaultProg = Program () (PLC.defaultVersion ())

data UPLCError ann
    = ParseErrorE ParserErrorBundle
    | UniqueCoherencyErrorE (UniqueError ann)
    | FreeVariableErrorE FreeVariableError
    deriving stock (Eq, Generic, Functor)
    deriving anyclass (NFData)
deriving stock instance (Show ann) => Show (UPLCError ann)

