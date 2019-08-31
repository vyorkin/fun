module Pretty (ppExpr) where

import Text.PrettyPrint (Doc, (<+>), text, render, parens)
import Syntax (Expr(..))

ppExpr :: Expr -> String
ppExpr = render . pp 0

class Pretty e where
  pp :: Int -> e -> Doc

instance Pretty Expr where
  pp _ Zero = text "0"
  pp _ Tr   = text "true"
  pp _ Fl   = text "false"
  pp l (Succ e)   = wrapped l $ text "succ"   <+> pp (l + 1) e
  pp l (Pred e)   = wrapped l $ text "pred"   <+> pp (l + 1) e
  pp l (IsZero e) = wrapped l $ text "iszero" <+> pp (l + 1) e
  pp l (If c t f) =
        text "if"   <+> pp l c
    <+> text "then" <+> pp l t
    <+> text "else" <+> pp l f

wrapped :: Int -> Doc -> Doc
wrapped n
  | n > 0 = parens
  | otherwise = id
