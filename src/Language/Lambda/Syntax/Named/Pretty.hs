module Language.Lambda.Syntax.Named.Pretty
    (
      prettyPrint
    ) where

import Text.PrettyPrint.Leijen

import Language.Lambda.Syntax.Named.Exp (Exp(Var,App,Lam,Letrec))

prettyParens :: Bool -> Doc -> Doc
prettyParens b d = if b then parens d else d

prettyP :: Show a => Int -> Exp a -> Doc
prettyP _ (Var a) = text $ show a
prettyP d (fun `App` arg) = prettyParens (d >= aPrec) $
    prettyP aPrec fun <> space <> prettyP (succ aPrec) arg
  where
    aPrec = 9
prettyP d (Lam name body) = prettyParens (d >= lPrec) $
    text "\\" <> text (show name) <> prettyP lPrec body
  where
    lPrec = 6
prettyP d (Letrec defs term) = prettyParens (d >= ltcPrec) $
    text "letrec " <> braces (pDefs defs) <> text " in " <> prettyP ltcPrec term
  where
    pDefs :: Show a => [(a, Exp a)] -> Doc
    pDefs = braces . cat . punctuate (semi) . fmap pDef
    pDef :: Show a => (a, Exp a) -> Doc
    pDef (name, term) = text (show name) <> text " = " <> prettyP ltcPrec term
    ltcPrec = 10


prettyPrint :: Show a => Exp a -> String
prettyPrint e = displayS (renderCompact (prettyP 0 e)) ""

instance Show a => Pretty (Exp a) where
    pretty = prettyP 0
