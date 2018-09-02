module Main where

import Control.Monad.Trans (liftIO)
import System.Console.Haskeline
  ( InputT
  , defaultSettings
  , getInputLine
  , outputStrLn
  , runInputT
  )

import Calculator
  ( Expr(Calc, Double, Error, Subst, Var)
  , parseToplevel
  )

eval :: [Expr] -> Expr -> IO [Expr]
eval vars expr = do
  print (vars, expr)
  return vars

process :: [Expr] -> String -> IO [Expr]
process vars line =
  case parseToplevel "<stdin>" line of
    Left err -> print err >> return vars
    Right expr -> eval vars expr

main :: IO ()
main = runInputT defaultSettings (loop [])
  where
  loop :: [Expr] -> InputT IO ()
  loop vars = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Leaving calci."
      Just ":q" -> outputStrLn "Leaving calci."
      Just input -> do
        vars' <- liftIO (process vars input)
        loop vars'

