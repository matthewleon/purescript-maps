module Bench.Data.StrMap where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (benchWith)

import Data.Tuple (Tuple(..))
import Data.List as L
import Data.Map as M

benchStrMap :: Eff (console :: CONSOLE) Unit
benchStrMap = benchFromFoldable
  where
  benchFromFoldable = do
    let natStrs = show <$> L.range 0 999999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList
