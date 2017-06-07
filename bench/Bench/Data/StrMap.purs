module Bench.Data.StrMap where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (benchWith)

import Data.Tuple (Tuple(..))
import Data.List as L
import Data.StrMap as M

benchStrMap :: Eff (console :: CONSOLE) Unit
benchStrMap = do
  log "fromFoldable"
  benchFromFoldable

  where
  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs

    log $ "fromFoldableArr (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldableArr natPairs
