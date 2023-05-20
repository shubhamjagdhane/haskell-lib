import Test.Validity
import Test.Hspec


main :: IO ()
main = putStrLn "Test suite not yet implemented"

describe :: Spec
describe = do
  describe "textCursorInsert" $
    it "produces valid text cursors" $
      producesValidsOnValids2 textCursorInsert


textCursorInsert :: Char -> TextCursor -> Maybe TextCursor
textCursorInsert '\n' _ = Nothing
textCursorInsert c tc = Just (tc & textCursorListCursorL %~ listCursorInsert c)

