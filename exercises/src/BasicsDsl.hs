{-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module BasicsDsl where

import Control.Monad.Except

import Control.Monad.Reader

import Data.Map.Strict as M
import Data.Text
import Text.Printf (vFmt)

type Name = String
type Val = Int
data Expr
  = Item Val
  | Add Expr Expr
  | Var Name
  | Let Name Expr Expr
  deriving (Show, Eq)

type Env = M.Map Name Val
type Eval = ReaderT Env (Except Text)

run :: Env -> Expr -> Either Text Val
run env expr = runExceptT (eval expr) env

eval :: (MonadReader Env m, MonadError Text m) => Expr -> m Val
eval (Item x) = pure x
eval (Add (Item a) (Item b)) = pure (a + b)
eval (Add va@(Var _) vb@(Var _)) = do
  a <- eval va
  b <- eval vb
  pure (a + b)
eval (Add a b) = throwError (pack ("expected Int, got " <> show a <> " and " <> show b))
eval (Var n) = do
  env <- ask
  case M.lookup n env of
    Just v -> pure v
    Nothing -> throwError (pack ("not found" <> n))
eval (Let n e1 e2) = do
  v <- eval e1
  local (M.insert n v) (eval e2)

example :: Expr =
  Let "x" (Item 10) $
    Let "y" (Item 32) $
      Add (Var "x") (Var "y")

runExample :: Either Text Val
runExample =
  run mempty example
