module Main where

import Data.Char(isDigit)
import System.IO (hPutStrLn, stderr, stdout)

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (text, vcat, hardline)

import Language.Lambda.Syntax.Named.Exp
import qualified Language.Lambda.Syntax.Named.Parser as Parser
import qualified Language.Lambda.Syntax.Named.Pretty as Pretty
import qualified Language.Lambda.Semantics.Named.BigStep as BS
import qualified Language.Lambda.Semantics.Named.SmallStep as SS

data Options = Options Mode Strategy Limit
    deriving (Show, Read, Eq)

data Mode = TraceNormalize | Trace | Normalize
    deriving (Show, Read, Eq)

data Strategy = NormalOrder | CallByName | CallByValue
    deriving (Show, Read, Eq)

data Limit = NoLimit | Limit Int
    deriving (Show, Read, Eq)

main :: IO ()
main = do
    opts <- getOpts
    input <- getContents
    output . process opts $ input

getOpts :: IO Options
getOpts = execParser $ info (helper <*> optionsP)
    (   fullDesc
    <>  header "lambda - interpreter for the untyped lambda calculus"
    <>  progDesc "Normalize and trace a lambda expression, \
                 \reading the expression from standard input, writing the \
                 \expression type to standard error, and writing \
                 \the normalized term to standard output."
    )

optionsP :: Parser Options
optionsP = Options <$> modeP <*> strategyP <*> limitP

modeP :: Parser Mode
modeP = option auto
    ( short 'm'
   <> long "mode"
   <> helpDoc (Just modeHelp)
   <> value TraceNormalize
   <> metavar "MODE"
    )
  where
    modeHelp = text "One of the following execution modes:" <> hardline <>
        vcat [ text ("* " ++ show TraceNormalize ++ " (Default)")
             , text ("* " ++ show Normalize)
             , text ("* " ++ show Trace)
             ]

strategyP :: Parser Strategy
strategyP = option auto
    ( short 's'
   <> long "strategy"
   <> helpDoc (Just strategyHelp)
   <> value NormalOrder
   <> metavar "STRATEGY"
    )
  where
    strategyHelp = text "One of the following evaluation strategies:" <> hardline <>
        vcat [ text ("* " ++ show NormalOrder ++ " (Default)")
             , text ("* " ++ show CallByValue)
             , text ("* " ++ show CallByName)
             ]

limitP :: Parser Limit
limitP = option (eitherReader parseLimit)
    ( short 'l'
   <> long "limit"
   <> helpDoc (Just limitHelp)
   <> value NoLimit
   <> metavar "LIMIT"
    )
  where
    limitHelp = text "Reduction step limit."
    parseLimit :: String -> Either String Limit
    parseLimit s
        | s == "UnLimit" = Right NoLimit
        | otherwise = if all isDigit s
                      then Right (Limit (read s))
                      else Left s

data Output = Output
    { toStdout :: Maybe String
    , toStderr :: Maybe String
    }

output :: Output -> IO ()
output (Output out err) = do
    op stderr err
    op stdout out
  where
    op h (Just s) = hPutStrLn h s
    op _ Nothing = return ()

process :: Options -> String -> Output
process opts stream = case Parser.expression stream of
    Left err    -> Output Nothing (Just (show err))
    Right expr  -> processExpr opts expr

processExpr :: Options -> Exp String -> Output
processExpr (Options mode strategy limit) expr = case mode of
    TraceNormalize  -> Output (Just (printRes smallStepResult)) (Just printDerivation)
    Trace           -> Output (Just printDerivation) Nothing
    Normalize       -> Output (Just (printRes bigStepResult)) Nothing
  where
    (smallStepResult,derivation) = case limit of
        NoLimit -> trace strategy expr
        Limit i -> traceLimit strategy i expr
    bigStepResult = eval strategy expr
    printRes = Pretty.prettyPrint
    printDerivation = unlines $ fmap Pretty.prettyPrint derivation

eval :: Strategy -> Exp String -> Exp String
eval NormalOrder    = BS.mkNormalOrder renderFresh
eval CallByName     = BS.mkCallByName  renderFresh
eval CallByValue    = BS.mkCallByValue renderFresh

trace :: Strategy -> Exp String -> (Exp String, [Exp String])
trace NormalOrder   = SS.mkNormalOrderTraced renderFresh
trace CallByName    = SS.mkCallByNameTraced  renderFresh
trace CallByValue   = SS.mkCallByValueTraced renderFresh

traceLimit :: Strategy -> Int -> Exp String -> (Exp String, [Exp String])
traceLimit NormalOrder  = SS.mkNormalOrderTracedLimit renderFresh
traceLimit CallByName   = SS.mkCallByNameTracedLimit  renderFresh
traceLimit CallByValue  = SS.mkCallByValueTracedLimit renderFresh

renderFresh :: (String, Int) -> String
renderFresh (n,i) = n ++ show i

