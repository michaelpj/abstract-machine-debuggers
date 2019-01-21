module REPL where

import           StateMachine

import           Control.Monad.State.Strict

import           Data.Text.Prettyprint.Doc

import           System.Console.Repline

import           Text.Read                  (readMaybe)

printState :: (MonadState s m, MonadIO m, Pretty s) => m ()
printState = do
  currentState <- get
  liftIO $ print $ pretty currentState

cmd :: (MonadState s m, MonadIO m, Read c, Pretty c, Pretty s) => StateMachine s c -> String -> HaskelineT m ()
cmd machine input = do
  currentState <- get
  command <- case readMaybe input of
    Just str -> pure str
    Nothing -> do
      liftIO $ putStrLn $ "Not a known command: " ++ input
      abort
  case runCommand machine command currentState of
    Just newState -> do
      put newState
      printState
    Nothing -> do
      liftIO $ putStrLn $ "No transition for command: " ++ show (pretty command)
      abort

opts :: (MonadState s m, MonadIO m, Pretty s) => [(String, Cmd m)]
opts =
  [
    ("state", const printState)
  ]

repl :: (Read c, Pretty c, Pretty s) => StateMachine s c -> s -> IO s
repl machine initialState = flip execStateT initialState $ evalRepl (pure "> ") (cmd machine) opts (Just ':') (Word $ const (pure [])) printState
