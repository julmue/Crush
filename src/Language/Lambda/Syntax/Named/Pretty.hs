module Language.Lambda.Syntax.Named.Pretty
    (
      prettyPrint
    ) where

import Prelude

import Text.PrettyPrint.HughesPJ

import Language.Lambda.Syntax.Named.Exp (Exp(Var,App,Lam,Letrec))

identifier :: Show a => a -> Doc
identifier = text . tail . init . show

prettyP :: Show a => Int -> Exp a -> Doc
prettyP _ (Var a) = identifier $ a
prettyP d (fun `App` arg) = maybeParens (d > aPrec) $
    prettyP aPrec fun <> space <> prettyP (succ aPrec) arg
  where
    aPrec = 9
prettyP d (Lam name body) = maybeParens (d > lPrec) $
    text "\\" <> identifier name <> text "." <> prettyP lPrec body
  where
    lPrec = 6
prettyP d (Letrec defs term) = maybeParens (d > ltcPrec) $
    text "letrec "
    <> (pDefs defs)
    $$ text "in " <> prettyP ltcPrec term
  where
    pDefs :: Show a => [(a, Exp a)] -> Doc
    pDefs = braces . cat . punctuate semi . fmap pDef
    pDef :: Show a => (a, Exp a) -> Doc
    pDef (name, term) = identifier name <> text " = " <> prettyP ltcPrec term
    ltcPrec = 10


prettyPrint :: Show a => Exp a -> String
prettyPrint = render . prettyP 0

-- instance Show a => Pretty (Exp a) where
--     pretty = prettyP 0
