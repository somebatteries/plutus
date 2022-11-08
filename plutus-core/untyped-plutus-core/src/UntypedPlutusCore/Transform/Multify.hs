{-# LANGUAGE LambdaCase #-}
module UntypedPlutusCore.Transform.Multify
    ( multify
    ) where

import UntypedPlutusCore.Core

import Control.Lens (transformOf)

multify :: Term name uni fun a -> Term name uni fun a
multify = transformOf termSubterms processTerm

processTerm :: Term name uni fun a -> Term name uni fun a
processTerm = \case
    (Apply annOuter (Apply _annInner f argsInner) argsOuter) ->
        Apply annOuter f (argsInner <> argsOuter)
    (LamAbs annOuter nsOuter (LamAbs _annInner nsInner t))   ->
        LamAbs annOuter (nsOuter <> nsInner) t
    t                                                        ->
        t
