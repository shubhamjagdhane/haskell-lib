{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TextCursor 
    ( TextCursor 
    , makeListCursor
    , makeTextCursor
    )
    where

import GHC.Generics (Generic)
import Data.Validity
import qualified Data.Text as T
import Data.Text.Internal


data ListCursor a = ListCursor
    { previous :: [a]
    , next :: [a]
    } deriving (Show, Eq, Generic)

newtype TextCursor = TextCursor
    { unTextCursor :: ListCursor Char }
    deriving (Show, Eq, Generic)

makeListCursor :: Char -> TextCursor 
makeListCursor as = TextCursor {unTextCursor = initListCursor as}

initListCursor :: Char -> ListCursor Char
initListCursor as = ListCursor { previous = [], next = [as]}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor lc = Prelude.reverse (previous lc) ++ next lc

instance Validity TextCursor where
    validate (TextCursor lc) = mconcat
        [ genericValidate lc
        , decorateList (rebuildListCursor lc) $ \c -> 
            declare "The character is not newline character" $ 
                c /= '\n'
        ]

makeTextCursor :: Text -> Maybe TextCursor
makeTextCursor = constructValid . makeListCursor . T.last
