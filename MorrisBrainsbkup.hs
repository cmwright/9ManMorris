-- The "brains" of the Nine Man Morris game.
-- This skeleton version contains "dummy" versions of
-- all of the functions that the main module expects
-- to use.  Students must fill in bodies for all the
-- functions required for the play level they're implementing.
-- You will need plenty of helper functions as well.
-- CISC 260, winter 2011
-- M. Lamb
module MorrisBrains where
import MorrisDefinitions

-- this constant defines the level at which we are playing 
-- (must be 1, 2 or 3)
playLevel :: Int
playLevel = 1 -- implement level 1 first, then progress to 2 & 3

---------------------------------------------------------------------
-- FUNCTIONS NEEDED FOR ALL THREE LEVELS
---------------------------------------------------------------------

-- Parameters: a board and a player character.  Assume the player is 
-- either humanChar or computerChar.
-- Return value: the number of mills the player has
-- (This function is technically not necessary until level 2, but
-- it's a very useful helper for level 1)
millCount :: Board -> Char -> Int
millCount (humanPositions,computerPositions) player = 0 -- dummy
    
-- Tests if the game is over.  Returns one of four characters:
--   humanChar: game is over and human player has won
--   computerChar: game is over and computer has won
--   'D': game is over and game is a draw (only possible in levels 1&2)
--   'X': game is not over
-- At levels 1 and 2, the game is over when neither player has a piece
-- left to play.  The winner is the player with the most mills.  It's a
-- draw if the two players have the same number of mills.
-- At level 3, the game is over when one player has less than 3 pieces
-- on the board or can't move any of their pieces.  That player loses.
-- There will be no draws at level 3.
gameOver :: GameState -> Char
gameOver state = 'X' -- dummy
    
   
-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 1 move to make (adding a piece to the board).
-- Return value: the position where the new piece should go.
-- Assumes the game is not over, so there will be a legal move.
bestMove1 :: GameState -> Int
bestMove1 state = 1 -- dummy

-- A new game state produced by placing a piece on the board
-- Parameters: initial state and position where piece will go.  The piece 
-- will be  taken from the player whose turn it is.  Assumes the player 
-- has at least one piece remaining and the position is free.
-- Returns: new game state.  The player does not change.
addPiece :: GameState -> Int -> GameState
addPiece state _ = state -- dummy      
      


---------------------------------------------------------------------
-- FUNCTIONS NEEDED FOR LEVELS 2&3 ONLY
-- (Level 1 will not use these functions, so the dummy
--  values won't affect the game)
---------------------------------------------------------------------

-- a new game state produced by removing a piece from the board
-- Parameters: initial state, and position from which to remove the piece
-- Assumes the position is currently occupied.  The player does not 
-- change.  This is not used by the main module until level 3, but
-- it's a good helper function for level 2 when capturing pieces.
-- Returns: new game state
removePiece :: GameState -> Int -> GameState
removePiece state _ = state -- dummy
    

-- Given a game state after a player has made a mill, returns a list of -- the opponent pieces it would be legal to capture.  These are all the -- pieces which are not part of a mill.  Exception: if there are no 
-- pieces outside a mill, then any piece may be captured.  
captureList :: GameState -> [Int]
captureList state = [1] -- dummy
    
-- Picks the best capture for the computer to make after a mill 
-- Parameters: starting state and list of possible captures (assume 
-- non-empty)
bestCapture :: GameState -> [Int] -> Int
bestCapture state positions = 1 -- dummy

    
---------------------------------------------------------------------
-- FUNCTION NEEDED FOR LEVEL 3 ONLY
-- (Levels 1&2 will not use this function, so the dummy
--  value won't affect the game)
---------------------------------------------------------------------

-- This function is like bestMove1, but for phase 2 of the game
-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 2 move to make (moving a piece to an adjacent position).
-- Return value: the best move
-- Assumes the game is not over, so there will be a legal move.
-- Strategy:
--    A. If there's a move that gets you a mill (even if you have to 
--       break up a mill to do it), that's the best move
--    B. Move a piece away from a mill, hoping to move it back on your 
--       next move
--    C. Pick the move that gives you the state with the best score, as 
--       in phase 1.
bestMove2 :: GameState -> Phase2Move
bestMove2 state = (1,2) -- dummy