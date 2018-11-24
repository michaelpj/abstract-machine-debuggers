module REPL where

import           StateMachine

import           Control.Monad.State.Strict

import           Data.Text.Prettyprint.Doc

import           System.Console.Repline

printState :: (MonadState s m, MonadIO m, Pretty s) => m ()
printState = do
  currentState <- get
  liftIO $ print $ pretty currentState

cmd :: (MonadState s m, MonadIO m, Read c, Pretty c, Pretty s) => StateMachine s c -> String -> m ()
cmd machine input = do
  currentState <- get
  let command = read input
  case runCommand machine command currentState of
    Just newState -> do
      put newState
      printState
    Nothing -> liftIO $ putStrLn $ "No transition for command: " ++ show (pretty command)

opts :: (MonadState s m, MonadIO m, Pretty s) => [(String, Cmd m)]
opts =
  [
    ("state", const printState)
  ]

repl :: (Read c, Pretty c, Pretty s) => StateMachine s c -> s -> IO s
repl machine initialState = flip execStateT initialState $ evalRepl "> " (cmd machine) opts (Word $ const (pure [])) printState
