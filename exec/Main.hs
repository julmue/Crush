module Main where

import Control.Exception
import Control.Monad.Except
import System.IO (hPutStrLn, stderr, stdout)

import Options.Applicative

import Language.Lambda.Syntax.Named.Exp
import qualified Language.Lambda.Syntax.Named.Parser as Parser
import qualified Language.Lambda.Syntax.Named.Pretty as Pretty
import qualified Language.Lambda.Semantics.Named.BigStep as BS
import qualified Language.Lambda.Semantics.Named.SmallStep as SS

import qualified Bound.Unwrap as BU

-- flags
data Mode = Default | Trace | Normalize deriving (Show, Eq)
data Strategy = NormalOrder | CallByName | CallByValue deriving (Show, Eq)
data Options = Options Mode Strategy deriving (Show, Eq)

data Output = Output
    { toStdout :: Maybe String
    , toStderr :: Maybe String
    }

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> parser)
        (   fullDesc
        <>  header "lambda - interpreter for the untyped lambda calculus"
        <>  progDesc "Normalize and trace a lambda expression, \
                     \reading the expression from standard input, writing the \
                     \expression type to standard error, and writing \
                     \the normalized term to standard output."
        )
    input <- getContents
    output . process opts $ input

mode :: Parser Mode
mode = subparser
        (   command "trace"
            (info (helper <*> pure Trace)
                (   fullDesc
                <>  header "lambda trace - Trace lambda expression evaluation"
                <>  progDesc "Trace the execution of a lambda expression,\
                             \reading the expression from standard input and \
                             \writing the derivation tree to standard output."
                )
            )
        <>  metavar "trace"
        )
    <|> subparser
        (   command "normalize"
            (   info (helper <*> pure Normalize)
                (   fullDesc
                <>  header "lambda normalize - Normalize a lambda expression"
                <>  progDesc "Reduce a lambda expression to normal form using \
                             \β-reduction, reading the program \
                             \from standard input and writing the normalized \
                             \program to standard output."
                ) )
        <>  metavar "normalize"
        )
    <|> pure Default

strategy :: Parser Strategy
strategy = pure NormalOrder

parser :: Parser Options
parser = Options <$> mode <*> strategy


output :: Output -> IO ()
output (Output out err) = do
    op stdout out
    op stderr err
  where
    op h (Just s) = hPutStrLn h s
    op _ Nothing = return ()

process :: Options -> String -> Output
process (Options mode strategy) stream = case Parser.expression stream of
    Left err        -> Output Nothing (Just (show err))
    Right expr -> case mode of
        Default     -> Output (Just prettyE) (Just prettyDerivation)
        Trace       -> Output (Just prettyDerivation) Nothing
        Normalize   -> Output (Just prettyE') Nothing
      where
        (e,derivation) = trace strategy expr
        prettyE = Pretty.prettyPrint e
        prettyDerivation = unlines . fmap Pretty.prettyPrint $ derivation
        e' = eval strategy $ expr
        prettyE' = Pretty.prettyPrint e'

eval :: Strategy -> Exp String -> Exp String
eval NormalOrder = BS.mkNormalOrder renderFresh
eval CallByName = BS.mkCallByName renderFresh
eval CallByValue = BS.mkCallByValue renderFresh

trace :: Strategy -> Exp String -> (Exp String, [Exp String])
trace NormalOrder = SS.mkNormalOrderTraced renderFresh
trace CallByName = SS.mkCallByNameTraced renderFresh
trace CallByValue = SS.mkCallByValueTraced renderFresh

renderFresh :: BU.Fresh String -> String
renderFresh f = BU.uname f ++ show (BU.fresh f)

