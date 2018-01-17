module Bench.Data.Map where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List)
import Data.List as L
import Data.Map as M
import Data.Tuple (Tuple(..))
import Performance.Minibench (bench, benchWith)

benchMap :: Eff (console :: CONSOLE) Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "toUnfoldable"
  log "------------"
  benchToUnfoldable

  log ""

  log "toAscUnfoldable"
  log "------------"
  benchToAscUnfoldable

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  where
  nats = L.range 0 999999
  natPairs = (flip Tuple) unit <$> nats
  singletonMap = M.singleton 0 unit
  smallMap = M.fromFoldable $ L.take 100 natPairs
  midMap = M.fromFoldable $ L.take 10000 natPairs
  bigMap = M.fromFoldable $ natPairs

  benchSize = do
    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchToUnfoldable = do
    log "toUnfoldable: singleton map"
    bench \_ -> M.toUnfoldable singletonMap :: List (Tuple Int Unit)

    log $ "toUnfoldable: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.toUnfoldable smallMap :: List (Tuple Int Unit)

    log $ "toUnfoldable: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.toUnfoldable midMap :: List (Tuple Int Unit)

    log $ "toUnfoldable: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.toUnfoldable bigMap :: List (Tuple Int Unit)

  benchToAscUnfoldable = do
    log "toAscUnfoldable: singleton map"
    bench \_ -> M.toAscUnfoldable singletonMap :: List (Tuple Int Unit)

    log $ "toAscUnfoldable: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.toAscUnfoldable smallMap :: List (Tuple Int Unit)

    log $ "toAscUnfoldable: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.toAscUnfoldable midMap :: List (Tuple Int Unit)

    log $ "toAscUnfoldable: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.toAscUnfoldable bigMap :: List (Tuple Int Unit)

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs
