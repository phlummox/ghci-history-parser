-- vim: ts=2 sw=2 et :

module GHCi.History where

-- | A line and column number in a file.
data FilePos = FilePos { lineNum :: Int, colNum :: Int }
  deriving (Eq, Show)


-- | the type of an item in @ghci's@ history.
data HistoryItem = HistoryItem {
      histStepNum :: Int
    , funcName :: String
    , fileName :: String
    , startPos :: FilePos
    , endPos :: FilePos
  }
  deriving (Eq, Show)

startLine = lineNum . startPos
startCol = colNum . startPos
endLine = lineNum . endPos
endCol = colNum . endPos



