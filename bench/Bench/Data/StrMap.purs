module Bench.Data.StrMap where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench)

import Data.Tuple (Tuple(..))
import Data.List as A
import Data.StrMap (StrMap)
import Data.StrMap as M

benchStrMap :: Eff (console :: CONSOLE) Unit
benchStrMap =
  let bigMap = M.fromFoldable $ (flip Tuple) unit <<< show <$> A.range 0 99999
  in do
    log "insertion benchmark"
    benchInsert bigMap

    log ""

    log "lookup benchmark"
    benchLookup bigMap

    log ""

    log "delete benchmark"
    benchDelete bigMap

benchInsert :: StrMap Unit -> Eff (console :: CONSOLE) Unit
benchInsert m = bench \_ -> M.insert "0" unit m

benchLookup :: StrMap Unit -> Eff (console :: CONSOLE) Unit
benchLookup m = bench \_ -> M.lookup "0" m

benchDelete :: StrMap Unit -> Eff (console :: CONSOLE) Unit
benchDelete m = bench \_ -> M.delete "0" m
