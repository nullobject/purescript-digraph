module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS())
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.Graph (graphSpec)

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS | e) Unit
main = run [consoleReporter] graphSpec
