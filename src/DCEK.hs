{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine with direction control.
--
-- Rather than having two transitions for computing and returning, the machine now
-- encodes the next kind of computation it expects to do in its state. This allows us
-- to clearly encode the switch between different modes in the transitions.
module DCEK where

import           CEK                       (Closure (..), Context, Frame (..))
import           Lambda
import           StateMachine

import           Data.Map                  ((!))
import qualified Data.Map                  as Map
import           Data.String
import           Data.Text.Prettyprint.Doc

data Control = Compute | Return
instance Pretty Control where
  pretty = \case
    Compute -> "computing"
    Return  -> "returning"

data State = State Control (Closure Exp) Context
instance Pretty State where
  pretty (State d c k) = pretty c <> ":" <+> pretty d <> "," <+> "continuing with" <+> pretty k

inject :: Exp -> State
inject e = State Compute (Closure e mempty) mempty

step :: Transition State
step = transition $ \(State d clos@(Closure c e) k) -> case d of
  Compute -> case c of
    Var n         -> Just $ State Return (e ! n) k
    Apply fun arg -> Just $ State Compute (Closure fun e) (ApplyArg (Closure arg e) : k)
    Lambda{}      -> Just $ State Return clos k
  Return -> case k of
    ApplyArg argClos : k'                          -> Just $ State Compute argClos (ApplyFun clos : k')
    ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State Compute (Closure body (Map.insert n clos funEnv)) k'
    ApplyFun (Closure _ _) : _                     -> error "This is impossible since applyFun can only contain lambda closures"
    []                                             -> Nothing -- terminal

continue :: Transition State
continue = repeatedly step

data Act = Step | Continue deriving (Show, Read)
instance Pretty Act where
  pretty = fromString . show

dcek :: StateMachine State Act
dcek = StateMachine $ \case
  Step     -> step
  Continue -> continue

