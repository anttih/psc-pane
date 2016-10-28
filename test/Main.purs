module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Node.Process as P

-- import Test.StrongCheck (Arbitrary(), quickCheck)
-- import Test.StrongCheck.Gen (ondOf, chooseInt)
-- 
-- newtype FixedWidthTerm = FixedWidthTerm Int
-- 
-- runFixedWidthTerm :: FixedWidthTerm -> Int
-- runFixedWidthTerm (FixedWidthTerm h) = h
-- 
-- instance arbFixedWidthTerm :: Arbitrary FixedWidthTerm where
--   arbitrary = do
--     h <- chooseInt 2 200
--     pure $ FixedWidthTerm h
-- 
-- newtype ArbPscError = ArbPscError PscError
-- 
-- instance arbError :: Arbitrary ArbPscError where
--   arbitrary = do
--     moduleName <- arbitrary
--     errorCode <- oneOf (pure "ImportError") (pure <$> ["TypesDoNotUnify", "SomeWarning", "OtherError"])
--     message <- pure [ "  Could not match type"
--                     , "    Array String"
--                     , "  with type"
--                     , "    String"
--                     , ""
--                     , "while trying to match type Array String"
--                     , "  with type String"
--                     , "while checking that expression parseMessage message"
--                     , "  has type String"
--                     , "in value declaration isForeignPscError"
--                     , ""
--                     , ""
--                     , "See https://github.com/purescript/purescript/wiki/Error-Code-" <> errorCode <> " for more information,"
--                     , "or to contribute content related to this error."
--                     ]
--     filename <-


main ∷ ∀ eff. Eff (process ∷ P.PROCESS, console ∷ CONSOLE | eff) Unit
main = do
  error "boom"
  void $ P.exit 0
  --quickCheck \h err ->
  --  runFixedWidthTerm h <= height err
