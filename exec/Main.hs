module Main where

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
                Right expr -> putStrLn . Pretty.prettyPrint . normalOrder $ expr


renderFresh :: BU.Fresh String -> String
renderFresh f = BU.uname f ++ show (BU.fresh f)

normalOrder :: Exp String -> Exp String
normalOrder = BS.mkNormalOrder renderFresh


