{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Except
import Control.Monad.Identity
import System.IO (FilePath, hFlush, stdout)
import qualified System.IO.Strict as IOStrict
import System.Environment (getArgs)

import Bound.Scope
import qualified Bound.Unwrap as BU

import qualified Lambda as L
import qualified Lambda.Named.Parser as LParser

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as ParsecString

-- -----------------------------------------------------------------------------
{- What is a progam at this point?
A program consists of a single definition:

    main = <expr>

Execution of the program:

1. parse the definitions: [defs]
2. check if its only one definition
3. check if the name is "main"
-- create a named lambda
4. create a "letrec": Letrec [expr] (Var "main")
-- translate to unnamed lambda term
5. hoist to fresh names
6. uname hoisted term
7. normalize term
8. name term
9. print result

-}

main :: IO ()
main = do
   args <- getArgs
   if null args then runRepl else runExe $ head args

-- -----------------------------------------------------------------------------j
-- REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

runRepl = undefined

runExe :: FilePath -> IO ()
runExe fp = do
    s <- IOStrict.readFile fp
    putStrLn . either show show . runProcess . process $ s

-- process :: String -> Process (L.Expr String)
process s = do
    def@(name, body) <- parse pExe s
    if name /= "main"
        then throwError MissingMain
        else return . L.normalize . L.hoistFresh $ (L.Letrec [def] (L.Var name))


testFile = IOStrict.readFile "./examples/cookedSingleExpr.lam"

lmda = do
    ep <- fmap (runProcess . process) testFile
    return $ either undefined id ep

pExe :: ParsecString.Parser (String, L.Expr String)
pExe = LParser.def

-- -----------------------------------------------------------------------------
data ProcessError =
      ParserFailed Parsec.ParseError
--    | TooManyDefinitions
    | MissingMain
    | DefaultError

instance Show ProcessError where
    show (ParserFailed err) = show err
    show (MissingMain) = "ProcessError: Executable needs a \'main\' function"
    show (DefaultError) = "ProcessError: DefaultError"

hoistError :: (e -> ProcessError) -> Either e a -> Process a
hoistError f = either (throwError . f) return

-- -----------------------------------------------------------------------------
type Process a = ExceptT ProcessError Identity a

runProcess :: Process a -> Either ProcessError a
runProcess = runIdentity . runExceptT


-- -----------------------------------------------------------------------------
parse :: ParsecString.Parser a -> String -> Process a
parse p = hoistError ParserFailed . Parsec.parse p "Parser"



-- -----------------------------------------------------------------------------
--f :: Process (IO String)
-- f = do
--     str <- readFile "./examples/cookedSingleExpr.lam"
--     return (parse str)



