module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.GraphSpec (graphSpec)

main :: Effect Unit
main = run [consoleReporter] graphSpec
