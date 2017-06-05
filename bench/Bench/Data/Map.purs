module Bench.Data.Map where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench)

import Data.Tuple (Tuple(..))
import Data.Array as A
import Data.Map (Map)
import Data.Map as M

benchMap :: Eff (console :: CONSOLE) Unit
benchMap =
  let bigMap = M.fromFoldable $ (flip Tuple) unit <$> A.range 0 99999
  in do
    log "insertion benchmark"
    benchInsert bigMap

    log ""

    log "lookup benchmark"
    benchLookup bigMap

    log ""

    log "delete benchmark"
    benchDelete bigMap

benchInsert :: Map Int Unit -> Eff (console :: CONSOLE) Unit
benchInsert m = bench $ \_ -> M.insert 0 unit m

benchLookup :: Map Int Unit -> Eff (console :: CONSOLE) Unit
benchLookup m = bench $ \_ -> M.lookup 0 m

benchDelete :: Map Int Unit -> Eff (console :: CONSOLE) Unit
benchDelete m = bench $ \_ -> M.delete 0 m
