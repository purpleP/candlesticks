module Main where

import Data.Aeson
import Data.Conduit
import Data.Conduit.Lift
import Data.Ord
import Control.Monad.State
import Data.ByteString.Lazy.Char8 as BL hiding (last, head, maximum, minimum, map)
import qualified Data.Vector as V
import qualified Data.Map.Lazy as Map
import GHC.Exts (groupWith)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit (await, yield, (.|))
import Data.ByteString.Char8 as BC hiding (last, head, maximum, minimum, map)
import qualified Data.Conduit.Binary as CB
import Data.JsonStream.Parser hiding ((.|))
import System.IO (stdin, stdout)
import Control.Lens


jsonParseValue :: Parser a -> Conduit BC.ByteString IO a
jsonParseValue parser = doParse $ runParser parser
    where
        doParse :: ParseOutput a -> Conduit BC.ByteString IO a
        doParse out = case out of
                        ParseYield value newOutput  -> do
                            yield value
                            doParse newOutput
                        ParseNeedData cont ->
                            awaitForever $ doParse . cont
                        ParseDone remaining -> return ()
                        ParseFailed err -> error err

first (_, _, x, _, _, _) = x
second (_, _, _, x, _, _) = x
third (_, _, _, _, x, _) = x
fourth (_, _, _, _, _, x) = x

group' :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
group' k vs = map (\g -> (k $ head g, g)) $ groupWith k vs

candle :: (Num a, Ord a) => V.Vector (Int, Int, a, a, a, a) -> (a, a, a, a)
candle vs = (V.maximum $ V.map first vs, V.minimum $ V.map second vs, third $ V.head vs, fourth $ V.last vs)

candles :: (Num a, Ord a) => [(Int, Int, a, a, a, a)] -> Map.Map Int (Map.Map Int (a, a, a, a))
candles vs = Map.fromList $ Prelude.map (\(k, g) ->  (k, Map.fromList $ Prelude.map (\(t, gt) -> (t, candle $ V.fromList gt)) $ group' (\(_, x, _, _, _, _) -> x) g)) $ group' (\(x, _, _, _, _, _) -> x) vs

update :: (Ord a) => Map.Map Int (Map.Map Int (a, a, a, a)) -> Map.Map Int (Map.Map Int (a, a, a, a)) -> Map.Map Int (Map.Map Int (a, a, a, a))
update new old = Map.unionWith (Map.unionWith (\(mm1, mn1, o1, c1) (mm2, mn2, o2, c2) -> (max mm1 mm2, min mn1 mn2, o2, max c1 c2))) old new

diff = Map.differenceWith d1
    where
        d1 x y = if Map.null diff' then Nothing else Just diff'
            where diff' = Map.differenceWith d2 x y
                    where
                        d2 x y = if x == y then Nothing else Just x


-- diff :: (Ord a) => Map.Map Int (Map.Map Int (a, a, a, a)) -> State (Map.Map Int (Map.Map Int (a, a, a, a))) (Map.Map Int (Map.Map Int (a, a, a, a)))
-- diff cs = do
--     old <- get
--     let new = update cs old
--     put new
--     return $ diff old new


-- diffConduit :: (Monad m, Ord a) => Conduit (Map.Map Int (Map.Map Int (a, a, a, a))) m (Map.Map Int (Map.Map Int (a, a, a, a)))
-- diffConduit = evalStateC Map.empty $ awaitForever $ \cs -> do
--     old <- get
--     let new = update cs old
--     put new
--     yield $ diff old new

candlesParser :: Parser (Int, Int, Int, Int, Int, Int)
candlesParser = arrayOf $ (,,,,,) <$> arrayWithIndexOf 0 value
                                  <*> arrayWithIndexOf 1 value
                                  <*> arrayWithIndexOf 2 value
                                  <*> arrayWithIndexOf 2 value
                                  <*> arrayWithIndexOf 2 value
                                  <*> arrayWithIndexOf 2 value

printCandles = forever $ do
    l <- liftIO $ BC.getLine
    let cs = candles $ parseByteString candlesParser l
    old <- get
    let new = update cs old
    put new
    let d = diff new old
    liftIO $ if Map.null d then return () else BL.putStrLn $ encode d

main = runStateT printCandles Map.empty


-- main = runConduit $ CB.sourceHandle stdin
--                  .| jsonParseValue ((arrayOf $ (,,,,,) <$> arrayWithIndexOf 0 value <*> arrayWithIndexOf 1 value <*> arrayWithIndexOf 2 value <*> arrayWithIndexOf 2 value <*> arrayWithIndexOf 2 value <*> arrayWithIndexOf 2 value) :: Parser (Int, Int, Int, Int, Int, Int))
--                  .| CC.map candles
--                  .| diffConduit
--                  .| CC.map (pack . show)
--                  .| CB.sinkHandle stdout
