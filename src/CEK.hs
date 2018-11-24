{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine.
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

data State = State Exp Env Context
instance Pretty State where
  pretty (State c e k) = pretty c <+> "in" <+> pretty e <+> "continuing with" <+> pretty k

inject :: Exp -> State
inject e = State e mempty mempty

compute :: Transition State
compute = transition $ \(State c e k) -> case c of
  Var n         -> let (Closure term e') = e ! n in Just $ State term e' k
  Apply fun arg -> Just $ State fun e (ApplyArg (Closure arg e) : k)
  Lambda{}      -> Nothing

return :: Transition State
return = transition $ \(State c e k) -> case k of
  ApplyArg (Closure arg argEnv) : k'             -> Just $ State arg argEnv (ApplyFun (Closure c e) : k')
  ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State body (Map.insert n (Closure c e) funEnv) k'
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

