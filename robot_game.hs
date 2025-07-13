data Direction = North | South | East | West deriving (Show, Eq)
type Pos = (Int, Int)
data Move = Move Int | Rotate String deriving (Show, Eq)
type State = (Direction, Pos)
type Obstacles = [Pos]

withinBounds :: Pos -> Bool
withinBounds (x,y) = x >= 0 && y >= 0 && x < 5 && y < 5

-- takes a list of obstacle positions, the initial robot state, a list of moves
-- returns the robot's final position
-- play :: Obstacles -> State -> [Move] -> Pos
-- play _ (_,(x,y)) [] = (x,y)
-- play obstacles state ((Move a):moves)=  
--     play obstacles newState moves
--     where 
--         newState = move obstacles state a
-- play obstacles state ((Rotate direction):moves)=  
--     play obstacles newState moves
--     where 
--         newState = rotate state direction

move :: Obstacles -> State -> Int -> (State,String)
move _ (dir,(x,y)) 0 = ((dir, (x,y)), "Moved")
move obstacles (dir, (x,y)) steps =
    if newStep `notElem` obstacles && newStep /= (x,y) then move obstacles (dir, newStep) (steps-1)
    else ((dir,(x,y)), state)
    where
        ((_,newStep), state) = moveOneStep (dir,(x,y)) 

obstacles = [(1,1), (2,2), (0,4), (3,3)]
moveOneStep :: State -> (State, String) 
moveOneStep (dir, (x,y)) =
    if withinBounds newStep && newStep `notElem` obstacles then ((dir, newStep), "Moved")
    else ((dir, (x,y)), "Blocked")
    where 
        (_,newStep) = if dir == North then (dir,(x,y + 1))
                    else if dir == South then (dir,(x, y - 1))
                    else if dir == West then (dir,(x -1,y))
                    else (dir,(x+1,y))

rotate :: State -> String -> State
rotate (North,(x,y)) direction =
    if direction == "left" then (West,(x,y))
    else (East,(x,y))
rotate (South,(x,y)) direction =
    if direction == "left" then (East,(x,y))
    else (West,(x,y))
rotate (East,(x,y)) direction =
    if direction == "left" then (North,(x,y))
    else (South,(x,y))
rotate (West,(x,y)) direction =
    if direction == "left" then (South,(x,y))
    else (North,(x,y))

startPlay state finalPos = do
    putStr "Please type in a move: 'move' / 'rotate': " 
    line <- getLine
    case line of 
        "move" -> do
            putStr "Enter number of steps: "
            steps <- getLine
            let ((newState,newPos), blockedOrNot) = move obstacles state (read steps :: Int)
            if newPos == finalPos then putStrLn "You've reached the final destination. Game over!"
            else 
                case blockedOrNot of
                    "Blocked" -> putStrLn "Hit obstacle! Please rotate"
                    _ -> do
                        putStrLn "======================================"
                        putStrLn "Current Position: "
                        putStrLn (show newState)
                        putStrLn (show newPos)
                        putStrLn "======================================"
                        startPlay (newState,newPos) finalPos
        "rotate" -> do
             putStr "Enter rotate direction: "
             dir <- getLine
             let (newState, newPos) = rotate state dir
             if newPos == finalPos then putStrLn "You've reached the final destination. Game over!"
                else do
                    putStrLn "======================================"
                    putStrLn "Current Position: "
                    putStrLn (show newState)
                    putStrLn (show newPos)
                    putStrLn "======================================"
                    startPlay (newState,newPos) finalPos
        _ -> do
            putStr "Invalid move. "
            startPlay state finalPos

main = do
    putStrLn "Game Start!"
    startPlay (North, (0,0)) (3,1)
