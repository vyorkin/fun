{-# LANGUAGE LambdaCase #-}

module Eval (eval, isVal) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Syntax (Expr (..))

eval :: Expr -> Maybe Expr
eval e = case nf e of
  x | isVal x -> Just x
    | otherwise -> Nothing

nf :: Expr -> Expr
nf e = fromMaybe e $ nf <$> step e

step :: Expr -> Maybe Expr
step = \case
  IsZero Zero -> Just Tr
  IsZero (Succ e) | isNum e -> Just Fl
  IsZero e -> IsZero <$> step e
  Succ e -> Succ <$> step e
  Pred Zero -> Just Zero
  Pred (Succ e) | isNum e -> Just e
  Pred e -> Pred <$> step e
  If Tr t _ -> Just t
  If Fl _ f -> Just f
  If e t f -> step e <&> \c -> If c t f
  _ -> Nothing

isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ e) = isNum e
isNum _ = False

isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal e | isNum e = True
isVal _ = False
