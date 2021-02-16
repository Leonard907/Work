{-
    Module: Reed.

    *** PART III (10 pt) ***

    Define a player that uses teh Reed opening and play against it. Is the Reed opening a good 
    opening? Write your answers in Reed.txt.
-}
module Players.Reed where

import           Action
import           Game
import           Players.Minimax
import           Types

-- Create a player that starts with the Reed opening. After that, you may use your minimax action or
-- the given action for DumbPlayer. 
-- [Hint 1: Use the variable 'turn' in Player.]
-- [Hint 2: Use 'wallTop' to get the walls you need.]
-- [Hint 3: Don't forget to check that the action is valid using 'validWallAction'.]
reedPlayerAction :: Board -> [Player] -> String -> Int -> Maybe Action
reedPlayerAction b ps str r | validWallAction g c3h = Just (Place c3h)
                            | validWallAction g f3h = Just (Place f3h)
                            | otherwise             = minimaxAction b ps str r
  where
    c3h = ((('c', 3), ('c', 4)), (('d', 3), ('d', 4)))
    f3h = ((('f', 3), ('f', 4)), (('g', 3), ('g', 4)))
    g   = Game b ps


-- We build a Reed player from a name, a starting cell, a number of walls, an array of winning
-- positions and 'commandToAction'.
makeReedPlayer :: String -> Cell -> Int -> [Cell] -> Player
makeReedPlayer n c rws wps = Player { name             = n
                                    , turn             = 1
                                    , currentCell      = c
                                    , remainingWalls   = rws
                                    , winningPositions = wps
                                    , isHuman          = False
                                    , chooseAction     = reedPlayerAction
                                    }
