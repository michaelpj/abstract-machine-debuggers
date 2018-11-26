{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine with direction control and breakpoints.
--
-- We add a number of notions:
--
--    * A break continuation. This breaks normal returning by there
--      being no transitions in the normal returning step for this continuation.
--    * A computation break node. This breaks normal computation by there being no
--      transitions in the normal computation step for this node.
--
-- With these two we can implement both @next@ and user-defined computation breakpoints.
module DCEBK where

import           BreakLambda
import           DCEK                      (Control (..))
import           StateMachine
import           Types

import           Data.Map                  ((!))
import qualified Data.Map                  as Map
import           Data.String
import           Data.Text.Prettyprint.Doc

-- We need to redefine all these as we've changed our expression type

type Value = Exp

data Closure a = Closure a Env
instance Pretty a => Pretty (Closure a) where
  pretty (Closure v e) = parens (pretty v <+> "in" <+> pretty e)

type Env = Map.Map Name (Closure Value)
instance Pretty Env where
  pretty e = pretty $ Map.assocs e

data Frame = ApplyFun (Closure Value)
           | ApplyArg (Closure Exp)
           | Break
instance Pretty Frame where
  pretty = \case
    ApplyFun clos -> "applyFun" <+> pretty clos
    ApplyArg clos -> "applyArg" <+> pretty clos
    Break         -> "break"

type Context = [Frame]

data State = State Control (Closure Exp) Context
instance Pretty State where
  pretty (State d c k) = pretty c <> ":" <+> pretty d <> "," <+> "continuing with" <+> pretty k

inject :: Exp -> State
inject e = State Compute (Closure e mempty) mempty

-- | Normal step.
step :: Transition State
step = transition $ \(State d clos@(Closure c e) k) -> case d of
  Compute -> case c of
    Var n         -> Just $ State Return (e ! n) k
    Apply fun arg -> Just $ State Compute (Closure fun e) (ApplyArg (Closure arg e) : k)
    Lambda{}      -> Just $ State Return clos k
    -- notably, no steps for breakpoints
    _             -> Nothing
  Return -> case k of
    ApplyArg argClos : k'                          -> Just $ State Compute argClos (ApplyFun clos : k')
    ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State Compute (Closure body (Map.insert n clos funEnv)) k'
    ApplyFun (Closure _ _) : _                     -> error "This is impossible since applyFun can only contain lambda closures"
    -- notably, no steps for breakpoints
    _                                              -> Nothing

continue :: Transition State
continue = repeatedly step

-- | Steps over breakpoints only.
stepBreak :: Transition State
stepBreak = transition $ \(State d clos@(Closure c e) k) -> case d of
  Compute -> case c of
    BreakCompute c' -> Just $ State d (Closure c' e) k
    _               -> Nothing
  Return -> case k of
    Break : k' -> Just $ State d clos k'
    _          -> Nothing

-- | "Completely" evaluates the current term. That is, continues evaluation until we would be about
-- to use the current continuation.
next :: Transition State
next = transition $ \(State d clos k) -> case d of
  -- add a break continuation and then run until we hit it (or another breakpoint)
  Compute -> runTransition continue (State d clos (Break : k))
  Return  -> Nothing

data Act = Step | Continue | StepBreak | Next deriving (Show, Read)
instance Pretty Act where
  pretty = fromString . show

dcebk :: StateMachine State Act
dcebk = StateMachine $ \case
  Step      -> step
  StepBreak -> stepBreak
  Next      -> next
  Continue  -> continue

