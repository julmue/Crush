module Language.Lambda.Syntax.Named.Pretty
    (
      prettyPrint
    ) where

import Prelude

import Text.PrettyPrint.HughesPJ

import Language.Lambda.Syntax.Named.Exp

identifier :: Show a => a -> Doc
identifier = text . tail . init . show

prettyP :: Show a => Int -> Exp a -> Doc
prettyP _ (Var a) = identifier $ a
prettyP d (fun `App` arg) = maybeParens (d > aPrec) $
    prettyP aPrec fun <> space <> prettyP (succ aPrec) arg
  where
    aPrec = 9
prettyP d (Lam n b) = maybeParens (d > lPrec) $
    text "λ" <> identifier n <> text "." <> prettyP lPrec b
  where
    lPrec = 6
prettyP d (Let def term) = maybeParens (d > ltPrec) $
    text "let "
    <> (pDef def)
    $$ text "in " <> prettyP ltPrec term
  where
    pDef :: Show a => (a, Exp a) -> Doc
    pDef (n, t) = identifier n <> text " = " <> prettyP ltPrec t
    ltPrec = 10

prettyPrint :: Show a => Exp a -> String
prettyPrint = render . prettyP 0

-- instance Show a => Pretty (Exp a) where
--     pretty = prettyP 0
