-- vim: ts=2 sw=2 et :

{-# LANGUAGE NoMonomorphismRestriction  #-}

{- | 
Grammar describing the output of ghci's @:history@ command,
usable by either Parsec or Text.ParserCombinators.ReadP.
-}   

module GHCi.History.Parse.Common 
where

import Parsing as P

import GHCi.History (
    HistoryItem(..)
  , FilePos(..) 
  )

-- | one or more space characters
someSpace = skipSome space

-- | a number – one or more digits
num :: Parser Int
num = read <$> many1 digit

-- | @negNum := \'-\' \<'num'\>@
negNum :: Parser Int
negNum = negate <$> (char '-' *> num)

-- | @numSpan := \<'num'\> \'-\' \<'num'\>@
numSpan:: Parser (Int, Int) 
numSpan = (,) <$> num <*> (char '-' *> num)

-- | @filePos := \'(\' \<'num'\> \',\' \<'num'\> \')\'@
filePos :: Parser FilePos
filePos = FilePos <$> (char '(' *> num) <*> (char ',' *> num <* char ')')

-- | @unabbrevPosRange := \'(\' \<'filePos'\> \')-(\' \<'filePos'\> \')\'@
unabbrevPosRange :: Parser (FilePos , FilePos )
unabbrevPosRange = (,) <$> filePos <*> (char '-' *> filePos)

-- | @abbrevPosRange :=  \<'num'\> \':\' \<'num'\> \'-\' \<'num'\>@
abbrevPosRange :: Parser (FilePos , FilePos )
abbrevPosRange = 
  let
    lineAndStartCol = try $ ( (,) :: Int -> Int -> (Int, Int)) <$> num <*> (char ':' *> num) <* (char '-') 
  in
    mkRange <$> lineAndStartCol <*> num
  where
    mkRange (ln, stCol) endCol = (FilePos ln stCol, FilePos ln endCol)

-- | @pointRange :=  \<'num'\> \':\' \<'num'\>@
--
-- A "point range" – i.e., a "range" whose start point and end point
-- are the same.
pointRange :: Parser (FilePos, FilePos )
pointRange = mkRange <$> num <*> (char ':' *> num)
  where
    mkRange ln stCol = (FilePos ln stCol, FilePos ln stCol)


-- | @'posRange' := \<'abbrevPosRange'\> | \<'pointRange'\> | \<'unabbrevPosRange'\>@
posRange = abbrevPosRange <|> pointRange <|> unabbrevPosRange

-- | string representing a filename. Can't contain \"@:@\".
filename :: Parser String
filename = many1 $ noneOf ":"


-- | discards terminal control codes
funcname :: Parser String
funcname = someSpace *> option "" (string "\ESC[1m") *> many1 (noneOf " \ESC") <* option "" (string "\ESC[0m") <* someSpace
 

-- | @histLine := \<'negNum'\> \':\' \<'funcname'\> \'(\' \<'filename'\> \':\' \<'posRange'\> \')\'@
histLine :: Parser HistoryItem
histLine = 
  mkHistLine <$> (negNum <* someSpace <* char ':') <*> funcname  <*> (char '(' *> filename <* char ':') <*> posRange <* char ')'
  where
    mkHistLine n func file (startPos, endPos) = HistoryItem n func file startPos endPos

-- | An empty history starts with the string \"Empty history\".
emptyHistory :: Parser [HistoryItem]
emptyHistory = const [] <$> string "Empty history." <* manyTill anyChar eof

-- | A non-empty history: one or more 'histLine's,
-- followed by the string \"@\<end of history\>@\", or \"@...@\" 
-- if ghci has cut off remaining entries due to length.
nonEmptyHistory = manyTill (histLine <* endOfLine) (string "<end of history>"  <|> string "..." )

-- | @'history' := \<'emptyHistory'\> | \<'nonEmptyHistory'\>@
history = emptyHistory <|> nonEmptyHistory

