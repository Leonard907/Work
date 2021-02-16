{-
    Module: Player.
    A few utility functions to obtain and update player data.
-}
module Player where 

import Data.Char
import Data.Maybe
import Types
import Constants
import Cell
import Action
import Board 

{-
    Related to 'currentCell'.
-}

-- Given a list of players and a cell, returns the player in that cell if any or 'Nothing' 
-- otherwise.
playerInCell :: [Player] -> Cell -> Maybe Player 
playerInCell [] _ = Nothing 
playerInCell (p:ps) c' 
    | currentCell p == c' = Just p 
    | otherwise = playerInCell ps c'

-- Similar to the one above but returns a boolean.
cellFree :: [Player] -> Cell -> Bool 
cellFree [] c = True
cellFree (p:ps) c = (currentCell p /= c) && cellFree ps c

-- Given a player, returns the cells adjacent to it.
adjacentCells :: Player -> [Cell]
adjacentCells p = cellsAroundInBoard (currentCell p)

-- Given a player, return the possible jump locations
adjacentJumps :: Player -> [Cell]
adjacentJumps p = [(c,r+n) | n<-[-2,2], r+n >= firstRow, r+n <= lastRow] ++ 
                  [(chr (ord c + n),r) | n<-[-2,2], ord c + n >= ord firstColumn, ord c + n <= ord lastColumn]
    where 
        c = fst (currentCell p)
        r = snd (currentCell p)

-- Given a player, return the possible diagonal moves
adjacentDiagonals :: Player -> [Cell]
adjacentDiagonals p = [(chr (ord c + i),r+j) | 
                        (i,j)<-[(-1,1),(-1,-1),(1,-1),(1,1)], 
                        r+i >= firstRow, r+i <= lastRow, ord c + j >= ord firstColumn, ord c + j <= ord lastColumn]
    where 
        c = fst (currentCell p)
        r = snd (currentCell p)

{-
    Useful checks.
-}

-- The player has won if it is in one of the winning positions specified when it was created.
hasWon :: Player -> Bool
hasWon p = (currentCell p) `elem` (winningPositions p)

-- Check if the player has got any walls left.
hasWallsLeft :: Player -> Bool 
hasWallsLeft p = remainingWalls p > 0

-- The player can move if the target cell is free.
canMove :: Player -> [Player] -> Step -> Bool 
canMove p ps (cs, ce) = 
    (currentCell p == cs) && (cellFree ps ce) && (isAdjacent cs ce)

-- Return the middle cell from a step
middleCell :: Step -> Maybe Cell 
middleCell ((c,r), (c',r')) 
    | c == c' && abs (r - r') == 2 = Just (c, div (r+r') 2)
    | r == r' && abs (ord c - ord c') == 2 = Just (chr $ div (ord c + ord c') 2, r)
    | otherwise = Nothing

-- Given a list of players and a step, return whether a jump is possible
ableToJump :: Player -> [Player] -> Step -> Bool 
ableToJump p ps step = (currentCell p == fst step) && (middleCell step /= Nothing) && (not (cellFree ps (fromJust (middleCell step)))) && (cellFree ps (snd step))

-- Given a list of players and a step, return whether a diagonal is possible
ableToDiagonal :: Player -> [Player] -> Step -> Bool 
ableToDiagonal p ps ((c,r) ,(c',r')) = (currentCell p == (c,r)) && (cellFree ps (c',r')) && abs (ord c - ord c') == 1 && abs (r - r') == 1

{-
    Updating the player.
-}

-- Adds one to Player variable turn.
nextTurn :: Player -> Player 
nextTurn p = p { turn = (turn p) + 1 }

-- Update current cell.
movePlayer :: Player -> Step -> Player 
movePlayer p (_, c') = p { currentCell = c' } 

-- Subtract one from the number of walls remaining.
useWall :: Player -> Player 
useWall p = p { remainingWalls = (remainingWalls p) - 1 }