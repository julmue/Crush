module Language.Lambda.Syntax.Named.Pretty where

-- instance Show a => Show (Expr a) where

-- showSExp :: show a => Int -> Exp a -> ShowS
-- showSExp d =
--     showsPrec _ (Var n) = showString (show n)
--     showsPrec d (e1 :@ e2) = showParen (d>predApp) $
--         showsPrec predApp e1 . showChar ' ' . showsPrec (succ predApp) e2
--       where
--         predApp = 9
--     showsPrec d (Lam n e) = showParen (d>predLam) $
--         showChar '\\' .
--         showString (show n) .
--         showString "->" . showChar ' ' . showsPrec predLam e
--       where
--         predLam = 6
-- -- I do not suppose this is right ...
--     showsPrec d (Letrec defs expr) = showParen False $
--         showString "letrec " .
--         showString (show defs) .
--         showString "in " .
--         showsPrec 9 expr
