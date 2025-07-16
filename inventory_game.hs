import Control.Monad.State


-- Test-based inventory tracker, each item is represented as a string (e.g. "potion", "sword")
-- inventory is a list of items

-- Each pickup item adds item to the inventory
-- Each use item removes the item if it exists, otherwise, the inventory is unchanged

type Inventory = [String]
data Action = Pickup String | Use String

-- This adds the item to the beginning of the inventory
pickup :: String -> State Inventory ()
pickup x = do
    currState <- get 
    put (x:currState)
    return ()

-- This removes one copy of the item from the inventory if present
-- Hint: use get, filter and put
use :: String -> State Inventory ()
use x = do
    n <- get 
    put (removeOne x n)
    return ()

removeOne :: String -> Inventory -> Inventory
removeOne _ [] = []
removeOne a (x:xs)
    | a == x = xs
    | otherwise = x: removeOne a xs

-- takes a list of actions and runs them in order
performActions :: [Action] -> State Inventory ()
performActions [] = return ()
performActions (action:actions) = do
    case action of 
        Pickup x -> pickup x
        Use x -> use x
    performActions actions

-- runs the game and start with an empty inventory and returns the final inventory after performing all the actions
runGame :: [Action] -> Inventory
runGame actions = execState (performActions actions) []
