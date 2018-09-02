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
  , Op(Divide, Minus, Plus, Times)
  , parseToplevel
  )

betaReduction :: [Expr] -> Expr -> Expr
betaReduction vars expr@(Calc op lhs rhs) =
  case (lhs, rhs) of
    (Error _, _) -> lhs
    (_, Error _) -> rhs
    (Var name, _) -> case lookupVar name vars of
      Nothing -> Error $ name ++ " is not defined"
      Just formula -> betaReduction vars $ Calc op formula rhs
    (_, Var name) -> case lookupVar name vars of
      Nothing -> Error $ name ++ " is not defined"
      Just formula -> betaReduction vars $ Calc op lhs formula
    (Calc {}, _) -> betaReduction vars $ Calc op (betaReduction vars lhs) rhs
    (_, Calc {}) -> betaReduction vars $ Calc op lhs (betaReduction vars rhs)
    (Double l, Double r) -> case op of
      Plus -> Double (l + r)
      Minus -> Double (l - r)
      Times -> Double (l * r)
      Divide -> Double (l / r)
betaReduction _ expr = expr

lookupVar :: String -> [Expr] -> Maybe Expr
lookupVar _ [] = Nothing
lookupVar name (Subst (Var name') formula:vars)
  | name == name' = Just formula
  | otherwise = lookupVar name vars

eval :: [Expr] -> Expr -> IO [Expr]
eval vars expr@(Error msg) =
  putStrLn msg >> return vars
eval vars expr@Calc {} =
  case betaReduction vars expr of
    Error msg -> putStrLn msg >> return vars
    Double value -> print value >> return vars
eval vars expr@(Double value) = do
  print value
  return vars
eval vars expr@(Subst (Var name) formula) =
  return $ Subst (Var name) (betaReduction vars formula):vars
eval vars expr@(Var name) =
  case lookupVar name vars of
    Nothing -> eval vars $ Error $ name ++ " is not defined"
    Just formula -> eval vars formula

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

