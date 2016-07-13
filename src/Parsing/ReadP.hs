-- vim: ts=2 sw=2 et :

{-# LANGUAGE RankNTypes #-}

{- |

"Text.ParserCombinators.ReadP" instance of 'Parsing'.

-}

module Parsing.ReadP where

import Parsing
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP ( readP_to_S )
import Data.Char as Ch (isDigit, isSpace)

-- | make 'ReadP.ReadP' an instance of 'Parsing'.
instance Parsing ReadP.ReadP where
  try             = id
  (<?>)           = const
  skipMany        = ReadP.skipMany
  skipSome        = ReadP.skipMany1
  unexpected      = const ReadP.pfail
  eof             = ReadP.eof
  notFollowedBy p = ((Just <$> p) ReadP.<++ pure Nothing)
    >>= maybe (pure ()) (unexpected . show)
  satisfy         = ReadP.satisfy
  char            = ReadP.char
  notChar c       = ReadP.satisfy (/= c)
  anyChar         = ReadP.get
  string          = ReadP.string
  digit           = ReadP.satisfy Ch.isDigit
  space           = ReadP.satisfy Ch.isSpace
  many1           = ReadP.many1
  manyTill        = ReadP.manyTill
  option          = ReadP.option
  noneOf cs       = ReadP.satisfy (`notElem` cs)
  endOfLine       = newline <|> crlf <?> "new-line"

-- | parse a line feed character
newline :: Parser Char
newline = char '\n' <?> "lf new-line"

-- | parse the seqence cr, lf, and return a line feed character
crlf :: Parser Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"

-- | run a 'Parser' computation using 'ReadP'.
--
-- @parse p filename s@ runs parser @p@ on the string @s@,
-- obtained from source @filePath@.
-- The @filePath@ is not used at all, and may be the empty
-- string, or even @undefined@. Returns either an error message, a 'String' ('Left') or a
-- value of type @a@ ('Right').
parse :: Parser a -> String -> String -> Either String a
parse p filename s = 
  case readP_to_S p s of
    [] -> Left "couldn't parse"
    [(res, rest)] -> Right res
    parses -> Left "ambiguous parse" 





