{-
    Module: Game.
    Functions used in the game loop to change the game state.
-}
module Game where

import Types
import Constants
import Action
import Board 
import Player 

{-
    'performAction' and helpers.
-}

-- The current player is the first element in the players list.
currentPlayer :: [Player] -> Player 
currentPlayer = head

-- The previous player is the last element in the players list.
previousPlayer :: [Player] -> Player 
previousPlayer = last

-- The player that just played goes to the back of the list. Used to change turn.
rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = [] 
rotatePlayers (p:ps) = ps ++ [p]

-- A step action is valid if the step is valid and no other player is in the target cell (canMove).
validStepAction :: Game -> Step -> Bool
validStepAction (Game b ps) s = (canMove (currentPlayer ps) ps s) && (validStep b s)

-- A jump action is valid if the step interval has a player
-- Uncomment below to enable jump action
-- validJumpAction :: Game -> Step -> Bool 
-- validJumpAction (Game b ps) s = ableToJump (currentPlayer ps) ps s

-- A diagonal action is valid if the destination is not occupied. 
-- Uncomment below to enable diagonal action
-- validDiagonalAction :: Game -> Step -> Bool 
-- validDiagonalAction (Game b ps) s = ableToDiagonal (currentPlayer ps) ps s

-- Generate all valid steps at a game state.
validSteps :: Game -> [Action]
validSteps g@(Game b ps) = map Move (filter (validStepAction g) steps)
    where  
        steps = let p = currentPlayer ps in makeSteps (currentCell p) (adjacentCells p)

-- Generate all valid jumps at a game state
-- Uncomment below to enable jump action
-- validJumps :: Game -> [Action]
-- validJumps g@(Game b ps) = map Move (filter (validJumpAction g) steps)
--     where  
--         steps = let p = currentPlayer ps in makeSteps (currentCell p) (adjacentJumps p)


-- A wall action is valid if the wall is valid and player has walls remaining.
validWallAction :: Game -> Wall -> Bool 
validWallAction (Game b ps) w = (hasWallsLeft (currentPlayer ps)) && (validWall b w)

-- Generate all valid walls at a game state.
validWalls :: Game -> [Action]
validWalls g = map Place (filter (validWallAction g) walls)
    where 
        walls = concat [[wallRight c, wallTop c] | c<-[(i, j) | i<-allColumns, j<-allRows]]

-- Generate all valid diagonals at a game state.
-- Uncomment below to enable diagonal action
-- validDiagonals :: Game -> [Action]
-- validDiagonals g@(Game b ps) = map Move (filter (validDiagonalAction g) steps)
--     where  
--         steps = let p = currentPlayer ps in makeSteps (currentCell p) (adjacentDiagonals p) 

-- Generate all valid actions at a game state.
-- Commented part includes the valid jump and diagonal actions
validActions :: Game -> [Action]
validActions g = (validSteps g) ++ (validWalls g) {- ++ (validJumps g) ++ (validDiagonals g) -}

-- Key function. Given a game and an action, checks the validity of the action and applies it to the
-- game, generating a new game.
-- commented part are the code that makes jump and diagonal action work
performAction :: Game -> Action -> Maybe Game
performAction g@(Game b (p:ps)) (Move s)
    | validStepAction g s = 
        Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    -- | validJumpAction g s = 
    --     Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    -- | validDiagonalAction g s = 
    --     Just (Game b (rotatePlayers ((movePlayer (nextTurn p) s):ps)))
    | otherwise = Nothing
performAction g@(Game b (p:ps)) (Place w)
    | validWallAction g w = 
        Just (Game (placeWall b w) (rotatePlayers ((useWall (nextTurn p)):ps)))
    | otherwise = Nothing