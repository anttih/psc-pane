module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process as P

import Test.Spec (describe, it)
import Test.Spec.Runner (run)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

import PscPane.Pretty (formatTestOutput)

main ∷ ∀ eff. Eff (process ∷ P.PROCESS, console ∷ CONSOLE | eff) Unit
main = run [consoleReporter] do
  describe "Formatting test results" do
    it "shows the last n lines that fit the window" do
      let output = "First\nline\nline\nline"
      shouldEqual (formatTestOutput 2 { stdErr: output, stdOut: ""}) "First\nline"
    it "shows stdout instead of stderr when stderr is empty" do
      shouldEqual (formatTestOutput 2 { stdErr: "", stdOut: "stdout"}) "stdout"
