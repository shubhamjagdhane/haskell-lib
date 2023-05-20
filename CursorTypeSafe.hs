module CursorTypeSafe where

-- reference : https://cs-syd.eu/posts/2018-10-28-cursor-list

-- A type-safe list cursor

data ListCursor a = ListCursor
    { previous :: [a] -- In reverse order
    , next :: [a]
    } deriving Show

-- The idea is that the cursor is represented as two stack of elements, and that these stacks are 
-- to be interpreted such that the user is looking between the top elements of the stacks

initListCursor :: ListCursor Char
initListCursor = ListCursor
    { previous = "xet yM"
    , next = "tual example"
    }

-- Using this defination, every possible value of a ListCursor a will be a valid representation of a list cursor

makeListCursor :: [a] -> ListCursor a
makeListCursor as = ListCursor { previous = [], next = as}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor lc = reverse (previous lc) ++ next lc

-- Writing functions to move around in the cursor becomes simpler too
-- we just pop from one stack and push onto the other

listCursorPrev :: ListCursor a -> Maybe (ListCursor a) 
listCursorPrev lc = case previous lc of 
                      [] -> Nothing
                      (l:ls) -> Just $ lc {previous = ls, next = l : next lc}

listCursorNext :: ListCursor a -> Maybe (ListCursor a) 
listCursorNext lc = case next lc of 
                      [] -> Nothing
                      (r:rs) -> Just $ lc {previous = r : previous lc, next = rs}
