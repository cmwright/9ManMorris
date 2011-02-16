-- Common types & definitions for the Nine Man Morris game.  
-- Supplied for Haskell programming project
-- Students should not change this module
-- CISC 260, Winter 2011
module MorrisDefinitions where

-- A game is played against a human player (who goes first) and the computer.
-- I'm using the initials 'H' (for the human) and 'C' (for the computer).
-- You may change them to two other characters, except don't use 'X' or 'D' or digits, as 
-- these are used for other purposes in the code and the output.
humanChar :: Char
humanChar = 'H'
computerChar :: Char
computerChar = 'C'
otherPlayer c
    | c == humanChar = computerChar
    | otherwise = humanChar

-- A board is described by a tuple of two lists: the player's squares and the 
-- computer's squares.  Both lists should contain integers in [1..24] with no 
-- duplicates and no integers in both lists.  (Functions don't need to
-- check this assumption.)
type Board = ([Int],[Int])

-- The state of a game at any time is described by a tuple of four items:
-- 1. person whose turn it is (humanChar or computerChar)
-- 2. the number of pieces white has not yet placed on the board 
-- 3. the number of pieces black has not yet placed on the board
-- 4. the board
type GameState = (Char, Int, Int, Board)

-- extract parts of a state
getPlayer :: GameState -> Char
getPlayer (player,_,_,_) = player
getBoard :: GameState -> Board
getBoard (_,_,_,board) = board
getHumanCount :: GameState -> Int
getHumanCount (_,hcount,_,_) = hcount
getCompCount :: GameState -> Int
getCompCount (_,_,ccount,_) = ccount
getHumanPositions :: GameState -> [Int]
getHumanPositions (_,_,_,(hpos,_)) = hpos
getCompPositions :: GameState -> [Int]
getCompPositions (_,_,_,(_,cpos)) = cpos
getEmptyPositions :: GameState -> [Int]
getEmptyPositions (_,_,_,(hpos,cpos)) =
    [x|x<-[1..24], not (elem x hpos), not (elem x cpos)]
phase1 :: GameState -> Bool
phase1 (_,hcount,ccount,_) = hcount > 0 || ccount > 0
phase2 :: GameState -> Bool
phase2 state = not (phase1 state)

-- a move in phase1 is a single integer: the position on which to put a piece
type Phase1Move = Int
-- a move in phase2 is a pair of integers: (position to move from, position to move to)
type Phase2Move = (Int,Int)

-- Given a game state, returns an equivalent game state where it's the other
-- player's turn
switchPlayer :: GameState -> GameState
switchPlayer (player, hcount, ccount, board)
    | player == humanChar = (computerChar, hcount, ccount, board)
    | otherwise = (humanChar, hcount, ccount, board)


-- The set of all the possible mills, each given as a 3-element list of 
-- positions.  Each mill is given in one order only.
mills :: [[Int]]
mills = [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15],[16,17,18],
         [19,20,21],[22,23,24],[1,10,22],[4,11,19],[7,12,16],[2,5,8],
         [17,20,23],[9,13,18],[6,14,21],[3,15,24]]
         
-- The set of all pairs of adjacent squares (meaning a piece can move 
-- directly from one to the other).  Each adjacent pair is given as a 
-- 2-element list.   Each pair is given in one order only.  Observation: 
-- two adjacent squares must belong to a mill.
adjacentSpaces :: [[Int]]
adjacentSpaces = [[x,y]|[x,y,_] <- mills] ++ [[x,y]|[_,x,y] <- mills]

-- adjacent x y is true if x and y are adjacent
adjacent :: Int -> Int -> Bool
adjacent x y = elem [x,y] adjacentSpaces || elem [y,x] adjacentSpaces




