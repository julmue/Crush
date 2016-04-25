module Main where

import System.IO (hPutStrLn, stderr, stdout)

import qualified Bound.Unwrap as BU
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (text, vcat, hardline)

import Language.Lambda.Syntax.Named.Exp
import qualified Language.Lambda.Syntax.Named.Parser as Parser
import qualified Language.Lambda.Syntax.Named.Pretty as Pretty
import qualified Language.Lambda.Semantics.Named.BigStep as BS
import qualified Language.Lambda.Semantics.Named.SmallStep as SS

data Mode = TraceNormalize | Trace | Normalize
    deriving (Show, Read, Eq)

data Strategy = NormalOrder | CallByName | CallByValue
    deriving (Show, Read, Eq)

data Options = Options Mode Strategy
    deriving (Show, Read, Eq)

main :: IO ()
main = do
    opts <- getOpts
    input <- getContents
    output . process opts $ input

getOpts :: IO (Options)
getOpts = execParser $ info (helper <*> options)
    (   fullDesc
    <>  header "lambda - interpreter for the untyped lambda calculus"
    <>  progDesc "Normalize and trace a lambda expression, \
                 \reading the expression from standard input, writing the \
                 \expression type to standard error, and writing \
                 \the normalized term to standard output."
    )

options :: Parser Options
options = Options <$> mode <*> strategy

mode :: Parser Mode
mode = option auto
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

strategy :: Parser Strategy
strategy = option auto
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
process (Options mode strategy) stream = case Parser.expression stream of
    Left err        -> Output Nothing (Just (show err))
    Right expr -> case mode of
        TraceNormalize  -> Output (Just prettyE) (Just prettyDerivation)
        Trace           -> Output (Just prettyDerivation) Nothing
        Normalize       -> Output (Just prettyE') Nothing
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

