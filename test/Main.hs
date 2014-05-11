module Main where

import System.Exit
import Test.HUnit

main :: IO ()
main = do
  Counts _ _ es fs <- runTestTT $ TestList [tests]
  case (es, fs) of
    (0,0) -> return ()
    _ -> exitFailure

tests :: Test
tests = "tests" ~: TestList []
