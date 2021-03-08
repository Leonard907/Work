module Main where

{-
The commented section is self-written tests that test the correctnenss of 
minimax and alpha beta pruning
-}

import           Test.Hspec

-- import           AlphaBetaTest
import           GenerateGameTreeTest
-- import           MinimaxPrimitive
import           OrderingTreeTest
import           Players.Minimax
import           PruneBreadthTest
import           PruneDepthTest
import           UtilityTest

main :: IO ()
main = hspec $ do
    describe "Part I.a QuickCheck test"  generateGameTreeQuickCheckTest
    describe "Part I.b unit tests"       orderingTreeUnitTests
    describe "Part I.b QuickCheck tests" orderingTreeQuickCheckTests
    describe "Part I.c unit tests"       pruneDepthUnitTests
    describe "Part I.c QuickCheck tests" pruneDepthQuickCheckTests
    describe "Part I.d unit tests"       pruneBreadthUnitTests
    describe "Part I.d QuickCheck tests" pruneBreadthQuickCheckTests
    describe "Part I.e unit tests"       utilityUnitTests
    -- describe "Part I.f unit tests"       primitiveMinimax
    -- describe "Part II unit tests"        abMinimax
