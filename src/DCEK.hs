{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | CEK machine with direction control.
module DCEK where

import           CEK                       (Closure (..), Context, Env,
                                            Frame (..))
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
  Return -> case k of
    ApplyArg (Closure arg argEnv) : k'             -> Just $ State Compute arg argEnv (ApplyFun (Closure c e) : k')
    ApplyFun (Closure (Lambda n body) funEnv) : k' -> Just $ State Compute body (Map.insert n (Closure c e) funEnv) k'
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

