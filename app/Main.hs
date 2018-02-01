module Main where

import Data.Aeson
import Data.Conduit
import Data.Ord
import qualified Data.Map.Lazy as Map
import GHC.Exts (groupWith)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit (await, yield, (.|))
import Data.ByteString.Char8 as BC hiding (last, head, maximum, minimum, map)
import qualified Data.Conduit.Binary as CB
import Data.JsonStream.Parser hiding ((.|))
import System.IO (stdin, stdout)


jsonParse :: Conduit BC.ByteString IO Value
jsonParse = doParse parseOutput
    where
        parseOutput :: ParseOutput Value
        parseOutput = runParser value

        doParse :: ParseOutput Value -> Conduit BC.ByteString IO Value
        doParse out = case out of
                        ParseYield value newOutput  -> do
                            yield value
                            doParse newOutput
                        ParseNeedData cont ->
                            awaitForever $ \i -> doParse (cont i)
                        ParseDone remaining -> return ()
                        ParseFailed err -> error err


makeCandle :: (Num a, Ord a) => [a] -> (a, a, a, a)
makeCandle vs = (maximum vs, minimum vs, head vs, last vs)



key (k, _, _) = k
val (_, _, v) = v
mnt (_, m, _) = m




process :: (Num a, Ord a) => [(String, Int, a)] -> [(Int, a, a, a, a)]
process = undefined
-- process = map map (\ (_, _, x) -> x) groupWith snd groupWith fst

main = runConduit $ CB.sourceHandle stdin .| jsonParse .| CC.map (pack . show) .| CB.sinkHandle stdout
