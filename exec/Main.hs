module Main where

import System.IO (hPutStrLn, stderr)

import Language.Lambda.Syntax.Named.Exp
import qualified Language.Lambda.Syntax.Named.Parser as Parser
import qualified Language.Lambda.Syntax.Named.Pretty as Pretty
import qualified Language.Lambda.Semantics.Named.BigStep as BS
import qualified Language.Lambda.Semantics.Named.SmallStep as SS

import Control.Exception
import Control.Monad.Except

import qualified Bound.Unwrap as BU

data Mode = Default | Trace | Normalize deriving (Show, Eq)

main :: IO ()
main = do
    let mode = Default
    case mode of
        Default -> do
            stream <- getContents
            case Parser.expression stream of
                Left err -> putStrLn (show err)
                Right expr -> do
                    let (e, es) = normalOrderTraced expr
                        e' = Pretty.prettyPrint e
                        es' = unlines . fmap Pretty.prettyPrint $ es
                    hPutStrLn stderr es'
                    putStrLn e'
        Trace -> undefined
        Normalize -> undefined


normalOrder :: Exp String -> Exp String
normalOrder = BS.mkNormalOrder renderFresh

normalOrderTraced :: Exp String -> (Exp String, [Exp String])
normalOrderTraced = SS.mkNormalOrderTraced renderFresh

renderFresh :: BU.Fresh String -> String
renderFresh f = BU.uname f ++ show (BU.fresh f)

