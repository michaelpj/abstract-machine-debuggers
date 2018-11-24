{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module StateMachine where

import           Control.Applicative
import           Control.Monad.State

newtype StateMachine s c = StateMachine { runStateMachine :: c -> Transition s }

runCommand :: StateMachine s c -> c -> s -> Maybe s
runCommand machine = runTransition . runStateMachine machine

-- | Transitions are stateful operations with no output which may fail.
-- '>>' gives us the composition of two transitions where both must be
-- valid for the composition to be valid.
type Transition s = StateT s Maybe ()

runTransition :: Transition s -> s -> Maybe s
runTransition = execStateT

transition :: (s -> Maybe s) -> Transition s
transition f = StateT $ \s -> do
  s' <- f s
  pure ((), s')

identity :: Transition s
identity = pure ()

repeatedly :: Transition s -> Transition s
repeatedly trans = trans >> (repeatedly trans <|> identity)
