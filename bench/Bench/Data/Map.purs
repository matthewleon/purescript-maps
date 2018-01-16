module Bench.Data.Map where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench, benchWith)

import Data.Tuple (Tuple(..))
import Data.List as L
import Data.Map as M

benchMap :: Eff (console :: CONSOLE) Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "keys"
  log "------------"
  benchKeys

  log ""

  log "values"
  log "------------"
  benchValues

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

  benchKeys = do
    log "keys: singleton map"
    bench \_ -> M.keys singletonMap

    log $ "keys: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.keys smallMap

    log $ "keys: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.keys midMap

    log $ "keys: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.keys bigMap

  benchValues = do
    log "values: singleton map"
    bench \_ -> M.values singletonMap

    log $ "values: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.values smallMap

    log $ "values: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.values midMap

    log $ "values: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.values bigMap

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natPairs
