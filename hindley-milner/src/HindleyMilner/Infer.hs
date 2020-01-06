{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module implements expression typing. It is the second step in the interpretation process
-- (after parsing in "Parse" and before evaluation in "Eval").
--
-- Type inference is done in two phases:
--
-- * /Discovery/, where we walk the AST, assign fresh type variables where appropriate, and emit
-- constraints.
-- * /Unification/, where we unify all types using the emitted constraints and output the resulting
-- expression's type.
--
-- Note that let bindings are generalized:
--
-- > let f x = x in if (f True) (f 1) 0 -- OK.
-- > let g f = if (f True) (f 1) 0 in let f x = x in g f -- Not OK ("generalized" once in @g f@).
module HindleyMilner.Infer (
  -- * Types
  Type(..), displayType, TypeVar(..),
  boolType, doubleType, stringType,
  -- * Inference
  infer, TypeEnv, boundTo, lookupType, TypeError(..), displayTypeError
) where

import HindleyMilner.Parse

import Control.Monad (when)
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.RWS.Strict (RWST, evalRWST)
import Control.Monad.State.Strict (StateT, evalState, evalStateT, get, gets, modify, put)
import Control.Monad.Writer (tell)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | The type inference monad, first pass. A second pass unifies the types.
type Discover a = RWST TypeEnv (Set Constraint) Int (Except TypeError) a

-- | The type of an expression.
data Type
  = VarT TypeVar -- ^ A type variable.
  | ConstT Text -- ^ A type constant (e.g. doubles or strings).
  | ArrowT Type Type -- ^ A function type.
  deriving (Eq, Ord)

infixr 5 `ArrowT`

-- | A type variable.
newtype TypeVar = TypeVar
  { unVar :: Int
  } deriving (Eq, Ord)

-- | A closed type signature.
data Scheme
  = ForAll [TypeVar] Type

-- | All known type signatures. We store signatures rather than types to be able to generalize them
-- appropriately at usage sites (i.e. generalize let bindings but keep function arguments as-is).
newtype TypeEnv = TypeEnv
  { unTypeEnv :: Map Iden Scheme
  } deriving (Monoid, Semigroup)

-- | Extracts a type from the environment.
lookupType :: Iden -> TypeEnv -> Maybe Type
lookupType iden (TypeEnv m) = case Map.lookup iden m of
  Just (ForAll _ tp) -> Just tp
  Nothing -> Nothing

-- | Creates a singleton 'TypeEnv' for the given type. The binding is as polymorphic as possible.
boundTo :: Type -> Iden -> TypeEnv
boundTo tp iden = envForScheme iden (ForAll (toList $ collect tp) tp) where
  -- Collects all type variables in a type.
  collect (VarT var) = Set.singleton var
  collect (ArrowT argTp bodyTp) = collect argTp `Set.union` collect bodyTp
  collect _ = Set.empty

-- | Creates a singleton 'TypeEnv' for the given scheme.
envForScheme :: Iden -> Scheme -> TypeEnv
envForScheme iden sch = TypeEnv $ Map.singleton iden sch

-- | Creates a singleton /non-polymorphic/ 'TypeEnv' for the given type.
envForType :: Iden -> Type -> TypeEnv
envForType iden tp = envForScheme iden (ForAll [] tp)

-- | Pretty-prints a type. All type variables are represented as @$N@, numbered starting from 1 in
-- the order they are displayed left to right (so @$1@, @$2@, ...).
displayType :: Type -> Text
displayType tp = evalState (go False tp) Map.empty where
  go _ (VarT (TypeVar n)) = gets (Map.lookup n) >>= \case
    Just txt -> pure txt
    Nothing -> do
      txt <- T.pack . ("$" ++) . show . (+1) <$> gets Map.size
      modify (Map.insert n txt)
      pure txt
  go _ (ConstT txt) = pure txt
  go wrap (ArrowT argTp bodyTp) = do
    argTxt <- go True argTp
    bodyTxt <- go False bodyTp
    let txt = T.concat [ argTxt, " -> ", bodyTxt ]
    pure $ if wrap then T.concat [ "(", txt, ")" ] else txt

boolType, doubleType, stringType :: Type
boolType = ConstT "Bool"
doubleType = ConstT "Double"
stringType = ConstT "String"

litType :: Lit -> Type
litType BoolL {} = boolType
litType DoubleL {} = doubleType
litType StringL {} = stringType

data Constraint
  = Type :~: Type
  deriving (Eq, Ord)

-- | Error thrown when an expression is ill-typed.
data TypeError
  = InfiniteType Type Type -- ^ Failed to unify a type which occurs in an arrow type.
  | UnificationFailure Type Type -- ^ Failed to match two types.
  | UnboundVariable Iden -- ^ Encountered an undefined identifier.

-- | Pretty-prints a type error.
displayTypeError :: TypeError -> Text
displayTypeError (InfiniteType tp1 tp2) =
  "cannot create the infinite type " <> displayType tp1 <> " = " <> displayType tp2
displayTypeError (UnificationFailure tp1 tp2) =
  "cannot unify " <> displayType tp1 <> " with " <> displayType tp2
displayTypeError (UnboundVariable iden) = "identifier " <> iden <> " was not declared"

-- | Generates a fresh type variable.
fresh :: Discover Type
fresh = do
  n <- get
  modify (+1)
  pure $ VarT $ TypeVar n

-- | Constrains two types to unify.
constrain :: Type -> Type -> Discover ()
constrain tp1 tp2 = tell $ Set.singleton $ tp1 :~: tp2

substitute :: Map TypeVar Type -> Type -> Type
substitute m tp@(VarT var) = Map.findWithDefault tp var m
substitute m (ArrowT argTp retTp) = ArrowT (substitute m argTp) (substitute m retTp)
substitute _ tp@ConstT{} = tp

-- | Discovers an expression's type, emitting the constraints necessary to later unify the type. All
-- type variables inside the returned type are guaranteed unique.
discover :: Expr -> Discover Type
discover (VarE iden) = asks (Map.lookup iden . unTypeEnv) >>= \case
  Just (ForAll vars tp) -> do
    subst <- Map.fromList <$> traverse (\var -> (var,) <$> fresh) vars
    pure $ substitute subst tp
  Nothing -> throwError $ UnboundVariable iden
discover (LitE lit) = pure $ litType lit
discover (AppE fn arg) = do
  fnArgTp <- fresh
  fnBodyTp <- fresh
  fnTp <- discover fn
  argTp <- discover arg
  constrain (ArrowT fnArgTp fnBodyTp) fnTp
  constrain fnArgTp argTp
  pure fnBodyTp
discover (LambdaE iden body) = do
  argTp <- fresh
  bodyTp <- local (envForType iden argTp <>) $ discover body
  pure $ ArrowT argTp bodyTp
discover (LetE (Binding refIden argIdens body) ret) = do
  args <- traverse (\iden -> (iden,) <$> fresh) argIdens
  refTp <- fresh
  let
    vars = (\(_, VarT var) -> var) <$> args
    env = mconcat (uncurry envForType <$> args) <> envForType refIden refTp
  bodyTp <- local (env <>) (discover body)
  constrain refTp bodyTp
  local (envForScheme refIden (ForAll vars refTp) <>) (discover ret)

-- | Checks whether a variable is present inside a type.
occurs :: TypeVar -> Type -> Bool
occurs var (VarT var') = var == var'
occurs var (ArrowT argTp bodyTp) = occurs var argTp || occurs var bodyTp
occurs _ ConstT{} = False

-- | The type used for applying constraints. Note that substitutions from constraints are applied
-- both on the type and the remaining constraints.
type Unify a = StateT (Type, Set Constraint) (Except TypeError) a

-- | Applies a constraint.
apply :: Constraint -> Unify ()
apply = updateState . sortedTypes where
  -- Extracts the constraint's types sorted to avoid duplication when pattern matching later on.
  sortedTypes (tp1 :~: tp2) = if tp2 < tp1 then (tp2, tp1) else (tp1, tp2)
  -- Updates the state's type and constraints to match the constraints.
  updateState (VarT var, tp@ConstT{}) = substituteVar var tp
  updateState (VarT var, tp@VarT{}) = substituteVar var tp
  updateState (tp@(VarT var), arrowTp@ArrowT{}) = if occurs var arrowTp
    then throwError $ InfiniteType tp arrowTp
    else substituteVar var arrowTp
  updateState (ArrowT argTp1 bodyTp1, ArrowT argTp2 bodyTp2) = do
    (tp, cs) <- get
    let newCs = Set.fromList [ (argTp1 :~: argTp2), (bodyTp1 :~: bodyTp2) ]
    put (tp, Set.union newCs cs)
  updateState (tp1, tp2) = when (tp1 /= tp2) $ throwError $ UnificationFailure tp1 tp2
  -- Substitutes a single var with a type.
  substituteVar var newTp = do
    let update = substitute $ Map.singleton var newTp
    (tp, cs) <- get
    put (update tp, Set.map (\(tp1 :~: tp2) -> (update tp1 :~: update tp2)) cs)

-- | Unifies all constraints and returns the unified type.
unify :: Unify Type
unify = do
  (tp, cs) <- get
  case Set.minView cs of
    Nothing -> pure tp
    Just (c, cs') -> put (tp, cs') >> apply c >> unify

-- | Infer an expression's type.
infer :: Expr -> TypeEnv -> Either TypeError Type
infer expr env = runExcept $ evalRWST (discover expr) env 1 >>= evalStateT unify
