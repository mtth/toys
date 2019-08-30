{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements expression evaluation.
module HindleyMilner.Eval (
  -- * Environment
  Env, envLookup,
  -- * Computations
  eval, Value(..),
  -- * Extending environments
  -- ** Safely
  bind,
  -- ** Unsafely
  unsafeEnv
) where

import HindleyMilner.Infer
import HindleyMilner.Parse

import Control.Monad.Fix (mfix)
import Control.Monad.Reader (Reader, ask, asks, local, runReader)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | The expression evaluation monad.
--
-- A few things to note:
--
-- * We don't return informative errors here (in fact we 'error' out...)  since all errors should be
-- handled at type inference time.
-- * The underlying reader monad is strict, we rely on the 'LazyValue' type below to enable lazy
-- evaluation.
type Eval a = Reader ValueEnv a

-- | A binding of identifier to values, in which we can evaluate expressions. Note that we use a
-- lazy map to allow recursive bindings.
type ValueEnv = Map Iden LazyValue

-- | A wrapper around values to allow both lazy evaluation and recursive bindings.
data LazyValue = Lazy { force :: Value }

-- | The result of evaluating an expression.
data Value
  = LitV Lit -- ^ A literal value.
  | ClosureV (Value -> Value) -- ^ A closure.
  | UndefinedV -- ^ Something went wrong.

-- | Adds a binding to the environment, returning the modified environment. Recursive bindings are
-- supported thanks to the 'LazyValue' indirection.
addBinding :: Binding -> Eval ValueEnv
addBinding (Binding iden _ expr) = do
  lazy <- mfix $ \lazy -> local (Map.insert iden (Lazy (force lazy))) $ evalExpr expr
  Map.insert iden lazy <$> ask

-- | Evaluates an expression.
evalExpr :: Expr -> Eval LazyValue
evalExpr (VarE iden) = asks (Map.lookup iden) >>= \case
  Just lazy -> pure lazy
  Nothing -> error "bug"
evalExpr (LitE lit) = pure $ Lazy $ LitV lit
evalExpr (AppE expr arg) = evalExpr expr >>= \case
  Lazy (ClosureV fn) -> Lazy . fn . force <$> evalExpr arg
  _ -> error "bug"
evalExpr (LambdaE iden body) = do
  env <- ask
  let fn val = force $ runReader (evalExpr body) (Map.insert iden (Lazy val) env)
  pure $ Lazy $ ClosureV fn
evalExpr (LetE binding expr) = do
  env <- addBinding binding
  local (const env) $ evalExpr expr

-- | An evaluation environment.
data Env = Env TypeEnv ValueEnv

instance Semigroup Env where
  (Env tpEnv1 valEnv1) <> (Env tpEnv2 valEnv2) = Env (tpEnv1 <> tpEnv2) (valEnv1 <> valEnv2)

instance Monoid Env where
  mempty = Env mempty Map.empty

-- | Looks up an identifier in the environment.
envLookup :: Iden -> Env -> Maybe (Type, Value)
envLookup iden (Env tpEnv valEnv) =
  (,) <$> lookupType iden tpEnv <*> (force <$> Map.lookup iden valEnv)

-- | Generates an environment with a single identifier bound. The type and value must match, no
-- checks are performed.
--
-- This is useful to add "built-in" functionality. For example to generate an environment with a
-- 'sin' operation, we could do something similar to:
--
-- > env = unsafeEnv "sin" (tp, ClosureV fn) where
-- >   tp = doubleType `ArrowT` doubleType
-- >   fn (LitV (DoubleL d)) = LitV $ DoubleL $ sin d
-- >   fn _ = UndefinedV
unsafeEnv :: Iden -> (Type, Value) -> Env
unsafeEnv iden (tp,val) = Env (tp `boundTo` iden) (Map.singleton iden (Lazy val))

-- | Adds a binding to an existing environment.
bind :: Binding -> Env -> Either TypeError Env
bind b (Env tpEnv valEnv) = let iden = bindingIden b in case infer (LetE b (VarE iden)) tpEnv of
  Left err -> Left err
  Right tp -> Right $ Env tpEnv' valEnv' where
    tpEnv' = (tp `boundTo` iden) <> tpEnv
    valEnv' = runReader (addBinding b) valEnv

-- | Evaluates an expression in the given environment.
eval :: Expr -> Env -> Either TypeError (Type, Value)
eval expr (Env tpEnv valEnv) = case infer expr tpEnv of
  Left err -> Left err
  Right tp -> case runReader (evalExpr expr) valEnv of
    Lazy val -> Right (tp,val)
