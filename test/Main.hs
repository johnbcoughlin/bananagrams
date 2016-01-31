module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import qualified EngineTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Bananagrams engine tests" EngineTest.testList
        ]
