module Bench.Data.Map where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench, benchWith)

import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Data.List as L
import Data.Map as M

benchMap :: Eff (console :: CONSOLE) Unit
benchMap = do
  log "size"
  log "---------------"
  benchSize

  log ""

  log "delete"
  log "------------"
  benchDelete

  log ""

  log "delete'"
  log "------------"
  benchDelete'

  log ""

  log "fromFoldable"
  log "------------"
  benchFromFoldable

  where

  nats = L.range 0 999999
  natPairs = (flip Tuple) unit <$> nats

  benchSize = do
    let singletonMap = M.singleton 0 unit
        smallMap = M.fromFoldable $ L.take 100 natPairs
        midMap = M.fromFoldable $ L.take 10000 natPairs
        bigMap = M.fromFoldable $ natPairs

    log "size: singleton map"
    bench \_ -> M.size singletonMap

    log $ "size: small map (" <> show (M.size smallMap) <> ")"
    bench \_ -> M.size smallMap

    log $ "size: midsize map (" <> show (M.size midMap) <> ")"
    benchWith 100 \_ -> M.size midMap

    log $ "size: big map (" <> show (M.size bigMap) <> ")"
    benchWith 10  \_ -> M.size bigMap

  benchDelete = do
    let midNats = L.take 10000 nats
        midNatsReverse = L.reverse midNats
        midMap = M.fromFoldable $ L.take 10000 natPairs

    log "delete: from lowest to highest"
    benchWith 10 \_ -> foldl (flip M.delete) midMap midNats

    log "delete: from highest to lowest"
    benchWith 10 \_ -> foldl (flip M.delete) midMap midNatsReverse

  benchDelete' = do
    let midNats = L.take 10000 nats
        midNatsReverse = L.reverse midNats
        midMap = M.fromFoldable $ L.take 10000 natPairs

    log "delete': from lowest to highest"
    benchWith 10 \_ -> foldl (flip M.delete') midMap midNats

    log "delete': from highest to lowest"
    benchWith 10 \_ -> foldl (flip M.delete') midMap midNatsReverse

  benchFromFoldable = do
    let natStrs = show <$> L.range 0 99999
        natStrPairs = (flip Tuple) unit <$> natStrs
        shortPairList = L.take 10000 natPairs

    log $ "fromFoldable (" <> show (L.length shortPairList) <> ")"
    benchWith 100 \_ -> M.fromFoldable shortPairList

    log $ "fromFoldable (" <> show (L.length natStrPairs) <> ")"
    benchWith 10 \_ -> M.fromFoldable natStrPairs
