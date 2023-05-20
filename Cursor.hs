module CursorLib where
import Data.Array (listArray)

data ListCursor a = ListCursor
    { list :: [a]
    , index :: Int
    } deriving Show

initCursor :: ListCursor Char
initCursor = ListCursor 
    { list = "shubham"
    , index = 10
    }

makeListCursor :: [a] -> ListCursor a
makeListCursor as = ListCursor {list = as, index=0}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor = list

-- we can start writing functions to look one character to the left or one character to the right
-- These are the functions that we would want to use to manipulate the editor state when a user 
-- presses the left and right arrows on their keyboard, respectively


listCursorPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorPrev lc 
    | index lc <= 0 = Nothing
    | otherwise = Just $ lc {index = index lc-1}


listCursorNext :: ListCursor a -> Maybe (ListCursor a)
listCursorNext lc 
    | index lc > (length  $ list lc) = Nothing
    | otherwise = Just $ lc {index = index lc+1}

-- the problem with this type is that there exist values that do not represent a valid cursor
-- ListCursor 
--     { list = "example"
--     , index = 20
--     }
--
--  This value is nonsensial. It somehow represents the idea that a cursor could be looking beyond the text in their editor
--
