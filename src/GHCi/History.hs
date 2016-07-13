-- vim: ts=2 sw=2 et :

{- |

Provides the 'HistoryItem' data type, representing an entry in ghci's history 
listing.

-}

module GHCi.History where

-- | Represents a position in a source file, line and column,
-- counting from 1. (Just as GHCi @:history@ output does.)
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

-- | same as @lineNum . startPos@
startLine = lineNum . startPos
-- | same as @colNum . startPos@
startCol = colNum . startPos
-- | same as @lineNum . endPos@
endLine = lineNum . endPos
-- | same as @colNum . endPos@
endCol = colNum . endPos



