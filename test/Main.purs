module Test.Main where

import Prelude

import Control.Coroutine (Consumer, consumer, runProcess, ($$))
import Control.Coroutine.Aff (close, emit, produceAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import PscPane.Main (mergeProducers)
import PscPane.Pretty (formatTestOutput)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main ∷ Effect Unit
main = run [consoleReporter] do
  describe "Formatting test results" do
    it "shows the last n lines that fit the window" do
      let output = "First\nline\nline\nline"
      shouldEqual (formatTestOutput 2 { stdErr: output, stdOut: ""}) "First\nline"
    it "shows stdout instead of stderr when stderr is empty" do
      shouldEqual (formatTestOutput 2 { stdErr: "", stdOut: "stdout"}) "stdout"

  describe "merging producers" do
    it "should yield twice" do
      let p1 = produceAff \emitter -> do
            emit emitter "p1: 1"
            emit emitter "p1: 2"
            close emitter unit

      let p2 = produceAff \emitter -> do
            emit emitter "p2: 1"
            emit emitter "p2: 2"
            close emitter unit

      let
        consume :: Consumer String Aff Unit
        consume = consumer \v -> do
            Console.log v
            pure Nothing

      runProcess (p1 `mergeProducers` p2 $$ consume)
      log "ended"