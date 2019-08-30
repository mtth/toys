{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements parsers for our language. It is the first step in the interpretation
-- process (followed by type inference in "Infer", and finally evaluation in "Eval"). More
-- specifically, this module is responsible for transforming 'Text' into expressions ('Expr').
--
-- Examples:
--
-- [Factorial 10] @let fact n = if (eq n 0) 1 (mul n (fact (add n (neg 1)))) in fact 10@
module HindleyMilner.Parse (
  -- * Expression types
  Expr(..), Iden, Lit(..), displayLit, Binding(..),
  -- * Parsing
  Parser, exprParser, bindingParser, ParsingError, displayParsingError
) where

import Control.Applicative ((<|>), empty, many)
import Control.Monad (void)
import Data.Scientific (toRealFloat)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL

-- | An expression.
data Expr
  = VarE Iden
  -- ^ A variable (e.g. @abc@).
  | LitE Lit -- 123 or "abc"
  -- ^ A literal value.
  | AppE Expr Expr
  -- ^ An application.
  | LambdaE Iden Expr
  -- ^ A lambda.
  | LetE Binding Expr
  -- ^ A let binding (e.g. @let f x = add x 2 in f 1@).
  deriving (Eq, Ord, Show)

-- | An identifier (e.g. variable name).
type Iden = Text

-- | A literal value.
data Lit
  = DoubleL Double
  | StringL Text
  | BoolL Bool
  deriving (Eq, Ord, Show)

displayLit :: Lit -> Text
displayLit (DoubleL d) = T.pack $ show d
displayLit (StringL t) = t
displayLit (BoolL b) = if b then "True" else "False"

-- | A bound identifier.
data Binding = Binding
  { bindingIden :: Iden -- ^ Bound reference.
  , bindingArgs :: [Iden] -- ^ Arguments (if the binding is a lambda).
  , bindingExpr :: Expr -- ^ Bound expression.
  } deriving (Eq, Ord, Show)

-- | A parser specialized to our implementation.
type Parser a = P.Parsec Void Text a

-- | Error thrown during parsing.
type ParsingError = P.ParseErrorBundle Text Void

-- | Pretty-prints a parsing error.
displayParsingError :: ParsingError -> Text
displayParsingError = T.pack . P.errorBundlePretty

whitespace :: Parser ()
whitespace = PL.space P.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = PL.lexeme whitespace

symbol :: Text -> Parser ()
symbol = void . PL.symbol whitespace

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

litParser :: Parser Lit
litParser = stringP <|> doubleP <|> boolP where
  quoteP = P.char '"'
  stringP = StringL . T.pack <$> (quoteP *> P.manyTill PL.charLiteral (lexeme quoteP))
  doubleP = DoubleL . toRealFloat <$> lexeme PL.scientific
  boolP = BoolL <$> (True <$ symbol "True" <|> False <$ symbol "False")

-- | Returns the list of reserved identifiers.
reserved :: Set Iden
reserved = Set.fromList ["let", "in"]

-- | Returns a parser for identifiers.
idenParser :: Parser Iden
idenParser =
  let
    pack h t = T.pack (h : t)
    check v = if Set.member v reserved then fail "reserved" else pure v
  in lexeme $ pack <$> P.lowerChar <*> many P.alphaNumChar >>= check

-- | Returns a parser for bindings (the subsection between @let@ and @in@). This parser is exported
-- to allow extending the built-in environment.
--
-- The syntax is @REF_IDEN [ARG_IDEN...] = BODY_EXPR@ where @REF_IDEN@ is the identifier of the
-- binding, @ARG_IDEN@ is the identifier of an argument (there can be 0 or more), and @EXPR_BODY@ is
-- the body of the binding.
bindingParser :: Parser Binding
bindingParser = do
  ref <- idenParser
  args <- many idenParser
  symbol "="
  body <- exprParser
  pure $ Binding ref args $ if null args then body else foldr LambdaE body args

-- | Returns an expression parser.
--
-- Five types of expressions are supported:
--
-- * /Identifiers:/ @IDEN@, must start with a lower-case letter then alpha-numeric characters
-- * /Literals:/ @LIT@ (numbers, @True@, @False@)
-- * /Function applications:/ @FN_IDEN ARG_IDEN@
-- * /Bindings:/ @let BINDING in EXPR@
-- * /Groups:/ @(EXPR)@
exprParser :: Parser Expr
exprParser = whitespace >> exprP where
  letP = LetE <$> P.between (symbol "let") (symbol "in") bindingParser <*> exprP
  varP = VarE <$> idenParser
  termP = parens exprP <|> P.try letP <|> P.try (LitE <$> litParser) <|> varP
  exprP = do
    t <- termP
    ts <- many $ P.try termP
    pure $ foldl AppE t ts
