{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for PLC terms in DefaultUni.

module PlutusCore.Parser
    ( module Export
    , program
    , parseProgram
    , parseTerm
    , parseType
    , SourcePos
    , ParserError(..)
    ) where

import PlutusCore.Annotation
import PlutusCore.Core (Program (..), Term (..), Type)
import PlutusCore.Default
import PlutusCore.Error (AsParserErrorBundle, ParserError (..))
import PlutusCore.MkPlc (mkIterApp, mkIterInst)
import PlutusCore.Name (Name, TyName)
import PlutusCore.Parser.Builtin as Export
import PlutusCore.Parser.ParserCommon as Export
import PlutusCore.Parser.Type as Export
import PlutusCore.Quote (MonadQuote)

import Control.Monad.Except (MonadError)
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (notFollowedBy), anySingle, choice, many, some, try)

-- | A parsable PLC term.
type PTerm = Term TyName Name DefaultUni DefaultFun SrcSpan

varTerm :: Parser PTerm
varTerm = withSpan $ \sp ->
    Var sp <$> name'

tyAbsTerm :: Parser PTerm
tyAbsTerm = withSpan $ \sp ->
    inParens $ TyAbs sp <$> (symbol "abs" *> tyName)  <*> kind <*> term

lamTerm :: Parser PTerm
lamTerm = withSpan $ \sp ->
    inParens $ LamAbs sp <$> (symbol "lam" *> name) <*> pType <*> term

appTerm :: Parser PTerm
appTerm = withSpan $ \sp ->
    inBrackets $ mkIterApp sp <$> term <*> some term

conTerm :: Parser PTerm
conTerm = withSpan $ \sp ->
    inParens $ Constant sp <$> (symbol "con" *> constant)

builtinTerm :: Parser PTerm
builtinTerm = withSpan $ \sp ->
    inParens $ Builtin sp <$> (symbol "builtin" *> builtinFunction)

tyInstTerm :: Parser PTerm
tyInstTerm = withSpan $ \sp ->
    inBraces $ mkIterInst sp <$> term <*> many pType

unwrapTerm :: Parser PTerm
unwrapTerm = withSpan $ \sp ->
    inParens $ Unwrap sp <$> (symbol "unwrap" *> term)

iwrapTerm :: Parser PTerm
iwrapTerm = withSpan $ \sp ->
    inParens $ IWrap sp <$> (symbol "iwrap" *> pType) <*> pType <*> term

errorTerm :: Parser PTerm
errorTerm = withSpan $ \sp ->
    inParens $ Error sp <$> (symbol "error" *> pType)

-- | Parser for all PLC terms.
term :: Parser PTerm
term = do
    whitespace
    choice $ map try
        [ tyAbsTerm
        , lamTerm
        , appTerm
        , conTerm
        , builtinTerm
        , tyInstTerm
        , unwrapTerm
        , iwrapTerm
        , errorTerm
        , varTerm
        ]

-- | Parse a PLC program. The resulting program will have fresh names. The
-- underlying monad must be capable of handling any parse errors.  This passes
-- "test" to the parser as the name of the input stream; to supply a name
-- explicity, use `parse program <name> <input>`.
parseProgram ::
    (AsParserErrorBundle e, MonadError e m, MonadQuote m)
    => Text
    -> m (Program TyName Name DefaultUni DefaultFun SrcSpan)
parseProgram = parseGen program

-- | Parser for PLC programs.
program :: Parser (Program TyName Name DefaultUni DefaultFun SrcSpan)
program = do
    whitespace
    prog <- withSpan $ \sp ->
        inParens $ Program sp <$> (symbol "program" *> version) <*> term
    notFollowedBy anySingle
    pure prog

-- | Parse a PLC term. The resulting program will have fresh names. The underlying monad
-- must be capable of handling any parse errors.
parseTerm :: (AsParserErrorBundle e, MonadError e m, MonadQuote m) =>
    Text -> m (Term TyName Name DefaultUni DefaultFun SrcSpan)
parseTerm = parseGen term

-- | Parse a PLC type. The resulting program will have fresh names. The underlying monad
-- must be capable of handling any parse errors.
parseType :: (AsParserErrorBundle e, MonadError e m, MonadQuote m) =>
    Text -> m (Type TyName DefaultUni SrcSpan)
parseType = parseGen pType
