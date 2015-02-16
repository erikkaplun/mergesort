{-# LANGUAGE NamedFieldPuns #-}

module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestProperties as Props

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
  qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 30, 
                                            Q.maxSize = 20} prop
  return $ (Finished . toTSResult) qres



tests :: IO [TS.Test]
tests = return [Test mergesortTest]
  where
    mergesortTest = TestInstance
        { run = runQuickCheck Props.propMergesort
        , name = "propMergesort"
        , tags = []
        , options = []
        , setOption = \_ _ -> Right mergesortTest
        }
