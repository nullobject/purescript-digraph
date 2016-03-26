module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Spec.Runner (Process, run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.Graph (graphSpec)

main :: forall e. Eff (console :: CONSOLE, process :: Process | e) Unit
main = run [consoleReporter] graphSpec
