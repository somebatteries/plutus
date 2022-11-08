-- editorconfig-checker-disable-file
module UntypedPlutusCore.MkUPlc (UVarDecl (..), uvarDeclName, uvarDeclAnn, mkVar, mkIterLamAbs, mkMultiLamAbs, Def(..), UTermDef) where

import Data.List
import Data.List.NonEmpty qualified as NE
import PlutusCore.MkPlc (Def (..), apply)
import UntypedPlutusCore.Core.Type

-- | A term definition as a variable.
type UTermDef name uni fun ann = Def (UVarDecl name ann) (Term name uni fun ann)

-- | Make a 'Var' referencing the given 'VarDecl'.
mkVar :: ann -> UVarDecl name ann -> Term name uni fun ann
mkVar ann = Var ann . _uvarDeclName

-- | Lambda abstract a list of names.
mkIterLamAbs
    :: [UVarDecl name ann]
    -> Term name uni fun ann
    -> Term name uni fun ann
mkIterLamAbs args body =
    foldr (\(UVarDecl ann name ) acc -> LamAbs ann (pure name) acc) body args

-- | Lambda abstract a list of names.
mkMultiLamAbs
    :: [UVarDecl name ann]
    -> Term name uni fun ann
    -> Term name uni fun ann
mkMultiLamAbs args body = case NE.nonEmpty args of
    Just vds ->
        let
            ann = _uvarDeclAnn $ NE.head vds
            vars = fmap (\(UVarDecl _ n) -> n) vds
        in LamAbs ann vars body
    Nothing -> body
