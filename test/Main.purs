module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.GraphSpec (graphSpec)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] graphSpec
