{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module exposes a higher-level API to run computations in our toy language. It also
-- provides various common built-in functions via 'defaultEnv'.
module HindleyMilner (
  -- * Interpreter
  interpret,
  Result(..),
  -- * Environment
  Env, defaultEnv, envLookup,
  -- ** Types
  Type(..), TypeVar, displayType,
  -- ** Values
  Value(..), Lit(..), displayLit,
  -- * Errors
  ParsingError, TypeError(..), displayTypeError
) where

import HindleyMilner.Eval
import HindleyMilner.Infer
import HindleyMilner.Parse

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (MonadState, get, gets, put)
import Data.Text (Text)
import qualified Text.Megaparsec as P

-- | The result of interpreting a command.
data Result
  = Computation Value -- ^ A pure value.
  | BoundIdentifier Iden Type -- ^ A binding was added to the environment.
  | InvalidSyntax ParsingError -- ^ The command could not be parsed.
  | InvalidType TypeError -- ^ The command was parsed but did not have a valid type.

-- | Returns a binding and expression parser.
parser :: Parser (Either Binding Expr)
parser = do
  eth <- (Left <$> P.try bindingParser) <|> (Right <$> exprParser)
  P.eof
  pure eth

-- | Interpret a command.
--
-- For example, the following will compute the value six:
--
-- > result = flip evalState defaultEnv $ do
-- >   interpret "x = 1"
-- >   interpret "y = add x 2" -- Assign y = x + 2 = 3.
-- >   interpret "mul x y" -- Return x * y = 6.
interpret :: MonadState Env m => Text -> m Result
interpret txt = case P.parse parser "" txt of
  Left err -> pure $ InvalidSyntax err
  Right cmd -> do
    env <- get
    case cmd of
      Left binding -> case bind binding env of
        Left err -> pure $ InvalidType err
        Right env' -> do
          put env'
          let iden = bindingIden binding
          gets (envLookup iden) >>= \case
            Just (tp,_) -> pure $ BoundIdentifier iden tp
            Nothing -> error "unreachable"
      Right expr -> case eval expr env of
        Left err -> pure $ InvalidType err
        Right (_, val) -> pure $ Computation val

-- | A default environment with bindings for common functionality:
--
-- * Numeric operations: @add@, @mul@, @neg@, ...
-- * Conditional and predicates: @if@, @eq@, @gt@, @lt@, ...
defaultEnv :: Env
defaultEnv = mconcat
  [ doubleBinOp "add" (+) DoubleL doubleType
  , doubleBinOp "sub" (-) DoubleL doubleType
  , doubleBinOp "mul" (*) DoubleL doubleType
  , doubleUnOp "neg" (\d -> -d) DoubleL doubleType
  , doubleBinOp "ge" (>=) BoolL boolType
  , doubleBinOp "le" (<=) BoolL boolType
  , doubleBinOp "gt" (>) BoolL boolType
  , doubleBinOp "lt" (<) BoolL boolType
  , doubleBinOp "eq" (==) BoolL boolType
  , boolBinOp "or" (||)
  , boolBinOp "and" (&&)
  , conditional ]

boolBinOp :: Iden -> (Bool -> Bool -> Bool) -> Env
boolBinOp iden fn = unsafeEnv iden (tp, ClosureV cls) where
  tp = boolType `ArrowT` boolType `ArrowT` boolType
  cls (LitV (BoolL b1)) = ClosureV $ \case
    LitV (BoolL b2) -> LitV $ BoolL $ fn b1 b2
    _ -> UndefinedV
  cls _ = UndefinedV

doubleBinOp :: Iden -> (Double -> Double -> a) -> (a -> Lit) -> Type -> Env
doubleBinOp iden fn end retTp = unsafeEnv iden (tp, ClosureV cls) where
  tp = doubleType `ArrowT` doubleType `ArrowT` retTp
  cls (LitV (DoubleL d1)) = ClosureV $ \case
    LitV (DoubleL d2) -> LitV $ end $ fn d1 d2
    _ -> UndefinedV
  cls _ = UndefinedV

doubleUnOp :: Iden -> (Double -> a) -> (a -> Lit) -> Type -> Env
doubleUnOp iden fn end retTp = unsafeEnv iden (tp, ClosureV cls) where
  tp = doubleType `ArrowT` retTp
  cls (LitV (DoubleL d)) = LitV $ end $ fn d
  cls _ = UndefinedV

conditional :: Env
conditional = unsafeEnv "if" (tp, ClosureV cls) where
  var = VarT $ TypeVar 1
  tp = boolType `ArrowT` var `ArrowT` var `ArrowT` var
  cls (LitV (BoolL b)) = ClosureV $ if b
    then \val -> ClosureV $ \_ -> val
    else \_ -> ClosureV $ \val -> val
  cls _ = UndefinedV
