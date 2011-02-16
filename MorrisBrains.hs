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
import Data.List

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
millCount (humanPositions,computerPositions) player
	| player == 'C' = millHelper mills computerPositions
	| otherwise = millHelper mills humanPositions

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
gameOver state
	| (getHumanCount state == 0) && (getCompCount state == 0) = compareMills (getBoard state)
	| otherwise = 'X'
    
-- Given a game state (assuming it's the computer's turn), pick the best 
-- legal phase 1 move to make (adding a piece to the board).
-- Return value: the position where the new piece should go.
-- Assumes the game is not over, so there will be a legal move.
bestMove1 :: GameState -> Int
bestMove1 state = (!!) openPositions bestIndex
	where
	openPositions = getEmptyPositions state
	compOutcomes = findPossibles openPositions (getCompPositions state)
	humanOutcomes = findPossibles openPositions (getHumanPositions state)
	bestIndex = doubleMaximize compOutcomes humanOutcomes


-- A new game state produced by placing a piece on the board
-- Parameters: initial state and position where piece will go.  The piece 
-- will be  taken from the player whose turn it is.  Assumes the player 
-- has at least one piece remaining and the position is free.
-- Returns: new game state.  The player does not change.
addPiece :: GameState -> Int -> GameState
addPiece (player, hCount, cCount, (humanPos, compPos)) newPiece
	| player == 'H' = ('H', hCount - 1, cCount, (newPositions, compPos))
	| otherwise = ('C', hCount, cCount - 1, (humanPos, newPositions))
	
	where 
	newPositions = getNewPositions player (humanPos, compPos) newPiece  
      


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

--Helper functions by Cody

--mill helper will recurse over a list of positions looking for mills
--returns number of instances in this list
millHelper :: [[Int]] -> [Int] -> Int
millHelper _ [] = 0
millHelper _ (x:[]) = 0
millHelper _ (x:y:[]) = 0
millHelper [] _ = 0
millHelper millList posList = checkMills mill posList + millHelper rest posList
	where
	(mill:rest) = millList
	
	
--check each mill possibility against player's current positions
--takes mills and current positions
--returns 1 for mill, 0 for not
checkMills :: [Int] -> [Int] -> Int
checkMills mill positions
	| (elem x positions) && (elem y positions) && (elem z positions) = 1
	| otherwise = 0
	where
	(x:y:z:nothing) = mill

--helper to ensure mill index is a triple
headTriple :: [[Int]] -> (Int, Int, Int)
headTriple (thisMill:rest) = (x, y, z)
	where
	(x:y:z:nothing) = thisMill

--compares the mills of each player to determine who won the game
compareMills :: Board -> Char
compareMills theBoard
	| playerMills > compMills = humanChar
	| playerMills < compMills = computerChar
	| otherwise = 'D'
	
	where 
	playerMills = millCount theBoard humanChar
	compMills = millCount theBoard computerChar
	
--take all of the empty positions and an array of the possible moves
--returns an array of the possible outcomes after this turn
findPossibles :: [Int] -> [Int] -> [[Int]]
findPossibles [] _ = []
findPossibles (empty:emptys) positions = [score] ++ findPossibles emptys positions
	where
	score = getScore positions empty emptys

--takes a players current positions, a new piece, and the list of empty positions
--returns a list containing the number of mill, the number of adjacents 	
getScore :: [Int] -> Int -> [Int] -> [Int]
getScore current possible emptys = [millScore, adjScore]
	where 
	millScore = millHelper mills (current ++ [possible])
	adjScore = getAdjacents current possible emptys

--takes current positions finds those that are adjacent, and also check that the third in the pair
--that would result in a mill is empty so that points are in order
--returns the number of legitimate adjacent pairs in the players positions
getAdjacents :: [Int] -> Int -> [Int] -> Int
getAdjacents current possible emptys = length (checkThird adjs emptys)
	where
	adjs = checkAdjs (current ++ [possible])

--takes an array of positions and returns those that are adjacent
checkAdjs :: [Int] -> [[Int]]
checkAdjs [] = [] --this shouldn't ever be reached but...
checkAdjs (x:[]) = []
checkAdjs (x:y:rest)
	| (adjacent x y == True) = [[x,y]] ++ checkAdjs (y:rest)
	| otherwise = checkAdjs (y:rest)

--takes a 2d array of adjacent positions 
--returns those positions that have an open third positions
checkThird :: [[Int]] -> [Int] -> [[Int]]
checkThird [] _ = []
checkThird (adj:rest) emptys
	| thirdOpen (one, two) mills emptys == True = adj : (checkThird rest emptys)
	| otherwise = checkThird rest emptys
	where (one:two:nothing) = adj

--checks whether the third of an adjacent pair is open or not
--takes the two adjacent spaces and the mills array from defns
thirdOpen :: (Int, Int) -> [[Int]] -> [Int] -> Bool
thirdOpen _ [] _ = False
thirdOpen (x, y) (mill:rest) emptys
--if both x and y are part of a mill and the difference between the mill and x and y is empty, then
--this is a legitimate adjacent pair for scoring
	| (elem x mill == True) && (elem y mill == True) && (elem missingPiece emptys == True) = True
	| otherwise = thirdOpen (x, y) rest emptys
	where 
	missingPiece = head ((\\) mill [x,y])


--takes a space on the board and the list of empty spaces
--returns whether it is open
isOpenSpace :: Int -> [Int] -> Bool
isOpenSpace space emptys = elem space emptys 

--takes 2 2d lists of possible scores
--returns the index where the computer's score is maximized
doubleMaximize :: [[Int]] -> [[Int]] -> Int
doubleMaximize compPos humanPos = getIndex together (maximum together) 0
	where
	compScores = scoreIt compPos 'C'
	humanScores = scoreIt humanPos 'H'
	together = bringTogether compScores humanScores
	
--takes a 2d list of integers and the char for which player
--returns an array of the sum of each inner list
scoreIt :: [[Int]] -> Char -> [Int]
scoreIt [] _ = []
scoreIt (index:indices) player
 	| player == 'C' = ((10 * x) + (4 * y)) : scoreIt indices player
	| otherwise = ((-9 * x) + (-5 * y)) : scoreIt indices player
	where
	(x:y:nothing) = index

--takes 2 lists, returns the lists summed together at each index
bringTogether :: [Int] -> [Int] -> [Int]
bringTogether [] _ = []
bringTogether _ [] = []
bringTogether lis1 lis2 = (x + y) : bringTogether rest1 rest2
	where
	(x:rest1) = lis1
	(y:rest2) = lis2

--takes a list and an int and 0 (for counting)- returns -1 if not in the list
--returns the index that the int resides at
getIndex :: [Int] -> Int -> Int -> Int
getIndex [] _ _ = -1
getIndex (x:xs) num count
	| x == num = count
	| otherwise = getIndex xs num (count + 1)

--takes the player, board, and the new piece to insert
--returns the players new list of positions
getNewPositions :: Char -> Board -> Int -> [Int]
getNewPositions player (humanPos, compPos) newPos
	| player == 'H' = insert newPos humanPos
	| otherwise = insert newPos compPos



