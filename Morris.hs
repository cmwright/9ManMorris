-- Top-level module for the Nine Man Morris game.  Contains I/O and the top-level
-- imperative logic. 
-- Supplied for Haskell programming project
-- Students should not change this module and do not need to understand
-- its details.
-- CISC 260, Winter 2011

module Morris where 
import MorrisDefinitions
import MorrisBrains
import Char

-- starting state: Human moves first, each player has 9 pieces, and the board is empty
initialState :: GameState
initialState = (humanChar, 9, 9, ([],[]))

-- Play the game from the initial state
morris :: IO ()
morris = do
    displayBoard (getBoard initialState)
    playGame initialState 
        
-- Play the game, starting with an arbitrary state (given as parameter)
playGame :: GameState -> IO ()
playGame state = 
    do
        let over = gameOver state 
        if (over == 'H') then 
                putStrLn "YOU WIN!"
            else if (over == 'C') then
                putStrLn "YOU LOSE!"
            else if (over == 'D') then
                putStrLn "GAME IS A DRAW"
            else if (playLevel < 3 || phase1 state) then
                playPhase1 state -- placing pieces on the board
            else
                playPhase2 state -- moving pieces, level 3 only

                
-- Plays the game assuming the next move will be placing a piece on the board 
-- (phase 1).  Parameter is the current game state      
playPhase1 :: GameState -> IO ()
playPhase1 state =
    if ((getPlayer state) == humanChar)  then do -- player's turn
            putStrLn "\nYOUR TURN"
            putStrLn ("you have " ++ (show (getHumanCount state))
                ++ " piece(s) left")
            position <- playerChoice (getBoard state)
            let nextState = addPiece state position
            putStrLn "\nthe board after your move:"
            displayBoard (getBoard nextState)
            if (playLevel >=2 && newMill state nextState) then do
                    newerState <- humanMill nextState
                    playGame (switchPlayer newerState)
                else
                    -- no capture, continue the game
                    playGame (switchPlayer nextState)
        else do -- computer's turn
            putStrLn "\nMY TURN"
            let newPosition = bestMove1 state
            putStrLn ("I am adding a piece to position " 
                      ++ (show newPosition)) 
            let nextState = addPiece state newPosition
            putStrLn "the board after my move:"
            displayBoard (getBoard nextState)
            if (playLevel >=2 && newMill state nextState) then do
                    newerState <- computerMill nextState
                    playGame (switchPlayer newerState)
                else
                    -- no capture, continue the game
                    playGame (switchPlayer nextState)
            

-- Gives the human player an opportunity to capture a piece after making 
-- a mill
-- Parameter: state of the game before capture
-- Return result: state of the game after capture
humanMill :: GameState -> IO GameState
humanMill state = do
    putStrLn "\nYOU MADE A MILL!"
    displayBoard (getBoard state)
    -- list of positions which may be captured
    let capList = captureList state
    if (null capList) then do
        -- the computer has no pieces on the board 
        -- (very unlikely, but check to be sure)
        putStrLn "I have no pieces on the board to be captured"
        return state
        else do  
            putStrLn ""
            capturedPos <- choosePosition "pick a position to capture" capList
            let newState = removePiece state capturedPos
            putStrLn "the board after your capture:"
            displayBoard (getBoard newState)
            return newState
        
        
-- Lets the computer capture a piece after making a mill
computerMill :: GameState -> IO GameState
computerMill state = do
    putStrLn "\nI MADE A MILL!"
    let capList = captureList state
    if (null capList) then 
            do
                -- human has no pieces on the board
                -- (very unlikely, but check)
                putStrLn "You have no pieces on the board to be captured"
                return state
        else do
            putStrLn ""
            let capturePosition = bestCapture state capList 
            putStrLn ("I will capture your piece in position " 
                      ++ (show capturePosition))
            let newState = removePiece state capturePosition
            putStrLn "the board after my capture:"
            displayBoard (getBoard newState)
            return newState
        

-- Plays the game assuming we are in phase 2, so we move pieces instead -- of placing them.
-- Parameter is starting state.
playPhase2 :: GameState -> IO ()
playPhase2 state = 
    if ((getPlayer state) == humanChar) then do -- human player's turn
            putStrLn "\nYOUR TURN"
            (fromPos,toPos) <- choosePhase2Move state
            let state2 = removePiece state fromPos
            let state3 = addPiece state2 toPos
            putStrLn "the board after your move:"
            displayBoard (getBoard state3)
            if (newMill state2 state3) then do
                    state4 <- humanMill state3
                    playGame (switchPlayer state4)
                else
                    playGame (switchPlayer state3)
        else do -- computer's turn
            putStrLn "\nMY TURN"
            let (fromPos, toPos) = bestMove2 state
            putStrLn ("I am moving from " ++ (show fromPos) ++ " to " 
                      ++ (show toPos))
            let state2 = removePiece state fromPos
            let state3 = addPiece state2 toPos
            putStrLn ("the board after my turn:")
            displayBoard (getBoard state3)
            if (newMill state2 state3) then do
                    state4 <- computerMill state3
                    playGame (switchPlayer state4)
                else
                    playGame (switchPlayer state3)
      
      
-- Takes two states, before & after a move.
-- Returns True if the move added a mill (i.e. second state has more --    mills)
newMill :: GameState -> GameState -> Bool
newMill state1 state2 = mills2 > mills1
    where
    player = getPlayer state1 -- the player who made the move
    mills1 = millCount (getBoard state1) player
    mills2 = millCount (getBoard state2) player
    
    
-- Prompts human player to pick an unoccupied position (without listing 
-- all the choices, since there may be many)
-- Parameter: the board 
-- Repeats until player picks a legal position.
-- Return value: the position chosen.
playerChoice :: Board -> IO Int
playerChoice (humans,computers) = do
    putStr "pick a position for your next piece: "
    input <- getLine
    if (not (allDigits input)) then do
            putStrLn "input must be a positive integer"
            choice <- (playerChoice (humans,computers))
            return choice
        else do
            let position = (read input) :: Int
            if (position < 1 || position > 24) then do
                    putStrLn "illegal position (must be in 1..24)"
                    choice <- (playerChoice (humans,computers))
                    return choice
                else if (elem position computers || elem position humans) then do
                    putStrLn "illegal choice: that position is already taken"
                    choice <- (playerChoice (humans,computers))
                    return choice
                else do
                    return position
            
            
-- Prompts player to pick a position from a list of choices
-- Parameters: a prompt and a list of choices
-- Return value: the player's choice, guaranteed to be in the list of 
-- choices
choosePosition :: String -> [Int] -> IO Int
choosePosition prompt choices = do
    putStr (prompt ++ ": " ++ show choices ++ "): ")
    input <- getLine
    if (not (allDigits input)) then do
            putStrLn "input must be a positive integer"
            choice <- choosePosition prompt choices
            return choice
        else do
            let position = (read input) :: Int
            if (not (elem position choices)) then do
                    putStrLn ((show position) ++ " is not one of the choices")
                    choice <- choosePosition prompt choices
                    return choice
                else do
                    return position
            
            
-- Prompts human player for a phase 2 move (from occupied position to 
-- unoccupied)
choosePhase2Move :: GameState -> IO (Int,Int)
choosePhase2Move state = do
    fromPos <- choosePosition "position to move from" (getHumanPositions state)
    toPos <- choosePosition "position to move to" (getEmptyPositions state)
    if not (adjacent fromPos toPos) then do
            putStrLn "these two positions are not adjacent"
            move <- choosePhase2Move state
            return move
        else
            return (fromPos, toPos)
        
        
-- Checks a string to make sure it consists only of digits
allDigits :: String -> Bool
allDigits str = and (map isDigit str)


-----------------------------------------------------------------------
-- A few things to help with debugging.  These aren't part of the game, 
-- but they may be useful while you're testing.
-----------------------------------------------------------------------

-- Display a game state
displayState :: GameState -> IO()
displayState (player, bcount, wcount, board) =
    do
        putStrLn ([player] ++ "\'s turn")
        putStrLn ("You have " ++ (show bcount) ++ " piece(s) left")
        putStrLn ("I have " ++ (show wcount) ++ " piece(s) left")
        displayBoard board
        
-- Display a list of game states, pausing after each to let the 
-- user see it.
displayStateList :: [GameState] -> IO()
displayStateList [] = putStrLn ""
displayStateList (state:states) =
    do
        displayState state
        putStrLn "\n" -- two blank lines
        putStr "type Enter to continue: "
        input <- getLine
        displayStateList states


--------------------------------------------------------------------
-- The rest of the module is some headache-making code for displaying 
-- the board.  You don't need to read it!  Just be aware of the
-- displayBoard function for debugging.
-----------------------------------------------------------------------
   
-- Displays a board on the screen.  Shows the result of the boardString 
-- function in a user-friendly way.
displayBoard :: Board -> IO()
displayBoard board = putStr (boardString board)


-- boardString returns a string to describe a board, where each occupied 
-- position is shown as computerChar or humanChar and each unoccupied 
-- position is shown as a number
boardString :: Board -> String
boardString board = concat (map combineStrings (zip displayStrings displayPadding))
    where
    -- a list of short strings representing the contents of each board 
    -- position
    displayStrings = map (positionStr board) [1..24]
    

-- A string to represent a position on the board.  This will be
-- "B" or "W" or a string form of the position if it is unoccupied.  
-- The result will be of length 1 or 2.
-- Parameters: a board and a position (int in 1..24)
positionStr :: Board -> Int -> String
positionStr (humans, computers) pos
    | elem pos humans = [humanChar]
    | elem pos computers = [computerChar]
    | otherwise = show pos
    

-- Combines a position string with the padding that comes after it.
-- If the position string is of length 1, includes the first character
-- of the padding twice.
-- Parameter: tuple containing a position string and a padding string
combineStrings :: (String,String) -> String
combineStrings (posStr,padding) = posStr ++ newPadding
    where
    newPadding = if null (tail posStr) then (head padding):padding
                                       else padding
    
-- For each position in the board, a string to print following that 
-- position.
-- If the character only takes one character to print, the first 
-- character of the display string is printed twice so things line up.
displayPadding :: [String]
displayPadding = [longGap, longGap, endNoBars++between1++startOneBar,
                  mediumGap, mediumGap, endOneBar++between2++startTwoBars,
                  shortGap, shortGap, endTwoBars++between3,
                  shortGap, shortGap, middleGap, shortGap, shortGap, 
                  endNoBars++between3++startTwoBars,
                  shortGap, shortGap, endTwoBars++between2++startOneBar,
                  mediumGap, mediumGap, endOneBar++between1,
                  longGap, longGap, endNoBars
                 ]
    where
    -- string to display after 1, 2, 22 and 23 (followed by long gaps)
    longGap = "----------"
    -- end of line after 3, 15 and 24 (last number on line with no '|'s 
    -- following)
    endNoBars = " \n"
    -- line to display after first row (1,2,3) and before last row (22,23,24) -- two bars
    between1 = "|           |           |\n"
    -- start of line before 3 and 19 (one vertical bar)
    startOneBar = "|   "
    -- string to display after 4, 5, 19 and 20 (followed by medium-sized gaps)
    mediumGap = "------"
    -- line to display after (4,5,6) row and before (19,20,21) -- five bars
    between2 = "|   |       |       |   |\n"
    -- start of line before 7 and 16 (two vertical bars)
    startTwoBars = "|   |   "
    -- end of line after 6 and 21 (one vertical bar)
    endOneBar = "  |\n"
    -- string to display after 7,8,10,11,13,14,16,17 (followed by short gaps)
    shortGap = "--"
    -- end of line after 9 and 18 (two vertical bars)
    endTwoBars = "  |   |\n"
    -- line to display before and after the (10,11,12,13,14,15] row
    between3 = "|   |   |       |   |   |\n"
    -- gap between 12 and 13 in the middle of the board
    middleGap = "      "
    
    

