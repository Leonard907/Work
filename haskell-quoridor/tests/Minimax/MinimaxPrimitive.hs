{-
    Module: PruneBreadthTest.

    Used to test Part I.d.
-}

module MinimaxPrimitive where

import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit       ( fromHUnitTest )
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Players.Minimax
import           Types
import           Util.Assertion
import           Util.StateTreeInstances

type TestTree = StateTree Int Action


testTree1 :: TestTree
testTree1 = StateTree
    0
    [ (Move (('a', 1), ('b', 2)), StateTree 3 ts)
    , (Move (('b', 1), ('c', 2)), StateTree 4 ts)
    , (Move (('c', 1), ('d', 2)), StateTree 0 ts)
    ]
  where
    ts =
        [ (Move (('d', 1), ('e', 2)), StateTree 5 [])
        , (Move (('e', 1), ('f', 2)), StateTree 7 [])
        , (Move (('f', 1), ('g', 2)), StateTree 1 [])
        ]

testTree2 :: TestTree
testTree2 = StateTree
    0
    [ (Move (('a', 1), ('k', 2)), StateTree 3 ts1)
    , (Move (('b', 1), ('c', 2)), StateTree 2 ts2)
    , (Move (('c', 1), ('d', 2)), StateTree 2 ts3)
    ]
  where
    ts1 =
        [ (Move (('d', 1), ('e', 2)), StateTree 3 [])
        , (Move (('e', 1), ('f', 2)), StateTree 12 [])
        , (Move (('f', 1), ('g', 2)), StateTree 8 [])
        ]
    ts2 =
        [ (Move (('d', 1), ('e', 2)), StateTree 9 [])
        , (Move (('e', 1), ('f', 2)), StateTree 4 [])
        , (Move (('f', 1), ('g', 2)), StateTree 6 [])
        ]
    ts3 =
        [ (Move (('d', 1), ('e', 2)), StateTree 14 [])
        , (Move (('e', 1), ('f', 2)), StateTree 5 [])
        , (Move (('f', 1), ('g', 2)), StateTree 2 [])
        ]

test1 :: Test
test1 = TestCase (assertEqual "Desired action" v (Move (('b', 1), ('c', 2))))
    where v = minimaxFromTree $ highFirst testTree1

test2 :: Test
test2 = TestCase (assertEqual "Desired action" v (Move (('a', 1), ('k', 2))))
    where v = minimaxFromTree $ highFirst testTree2

primitiveMinimax :: Spec
primitiveMinimax = fromHUnitTest $ TestList
    [ TestLabel "testMinimaxPrimitiveCorrectness" test1
    , TestLabel "testMinimaxPrimitiveCorrectness" test2
    ]
