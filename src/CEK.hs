{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine.
--
-- A few differences from the standard presentation:
--
--     * Traditionally the CEK machine distinguishes whether it should be computing or
--       returning based on whether the current term is a value or not. Instead we allow
--       both computing and returning transitions regardless, but our "step" transition
--       simply always prefers to compute, which gives the same behaviour.
--     * Traditionally the CEK machine stores "closures" in environments but computes terms
--       along with an environment. Saying that the machine in fact computes closures
--       simplifies the presentation.
module CEK where

import           Lambda
import           StateMachine
import           Types

import           Prelude                   hiding (return)

import           Control.Applicative

import           Data.Map                  ((!))
import qualified Data.Map                  as Map
import           Data.String
import           Data.Text.Prettyprint.Doc

type Value = Exp

data Closure a = Closure a Env
instance Pretty a => Pretty (Closure a) where
  pretty (Closure v e) = parens (pretty v <+> "in" <+> pretty e)

type Env = Map.Map Name (Closure Value)
instance Pretty Env where
  pretty e = pretty $ Map.assocs e

data Frame = ApplyFun (Closure Value)
           | ApplyArg (Closure Exp)
instance Pretty Frame where
  pretty = \case
    ApplyFun clos -> "applyFun" <+> pretty clos
    ApplyArg clos -> "applyArg" <+> pretty clos

type Context = [Frame]

data State = State (Closure Exp) Context
instance Pretty State where
  pretty (State clos k) = pretty clos <> ":" <+> "continuing with" <+> pretty k

inject :: Exp -> State
inject e = State (Closure e mempty) mempty

compute :: Transition State
compute = transition $ \(State (Closure c e) k) -> case c of
  Var n         -> Just $ State (e ! n) k
  Apply fun arg -> Just $ State (Closure fun e) (ApplyArg (Closure arg e) : k)
  Lambda{}      -> Nothing

return :: Transition State
return = transition $ \(State clos k) -> case k of
  ApplyArg argClos : k'                          -> Just $ State argClos (ApplyFun clos : k')
  ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State (Closure body (Map.insert n clos funEnv)) k'
  ApplyFun (Closure _ _) : _                     -> error "This is impossible since applyFun can only contain lambda closures"
  []                                             -> Nothing

step :: Transition State
step = compute <|> return

continue :: Transition State
continue = repeatedly step

data Act = Compute | Return | Step | Continue deriving (Show, Read)
instance Pretty Act where
  pretty = fromString . show

cek :: StateMachine State Act
cek = StateMachine $ \case
  Compute  -> compute
  Return   -> return
  Step     -> step
  Continue -> continue

