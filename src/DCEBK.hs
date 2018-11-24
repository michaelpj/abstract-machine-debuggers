{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine with direction control and breakpoints.
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

data State = State Control Exp Env Context
instance Pretty State where
  pretty (State d c e k) = pretty c <+> pretty d <+> "in" <+> pretty e <+> "continuing with" <+> pretty k

inject :: Exp -> State
inject e = State Compute e mempty mempty

step :: Transition State
step = transition $ \(State d c e k) -> case d of
  Compute -> case c of
    Var n         -> let (Closure term e') = e ! n in Just $ State Return term e' k
    Apply fun arg -> Just $ State Compute fun e (ApplyArg (Closure arg e) : k)
    Lambda{}      -> Just $ State Return c e k
    -- notably, no steps for breakpoints
    _             -> Nothing
  Return -> case k of
    ApplyArg (Closure arg argEnv) : k'             -> Just $ State Compute arg argEnv (ApplyFun (Closure c e) : k')
    ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State Compute body (Map.insert n (Closure c e) funEnv) k'
    ApplyFun (Closure _ _) : _                     -> error "This is impossible since applyFun can only contain lambda closures"
    -- notably, no steps for breakpoints
    _                                              -> Nothing

continue :: Transition State
continue = repeatedly step

stepBreak :: Transition State
stepBreak = transition $ \(State d c e k) -> case d of
  Compute -> case c of
    BreakCompute c' -> Just $ State d c' e k
    BreakReturn c'  -> Just $ State d c' e (Break : k)
    _               -> Nothing
  Return -> case k of
    Break : k' -> Just $ State d c e k'
    _          -> Nothing

next :: Transition State
next = transition $ \(State d c e k) -> case d of
  -- add a break continuation and then run until we hit it (or another breakpoint)
  Compute -> runTransition continue (State d c e (Break : k))
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

