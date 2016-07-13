-- vim: ts=2 sw=2 et :

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts,  
             RankNTypes  #-}

{- |

"Text.Parsec" instance of 'Parsing'.

-}

module Parsing.Parsec where


import Parsing
import qualified Text.Parsec as Parsec
import Text.Parsec.Error (
    messageString
  , errorPos
  , errorMessages 
  )

-- | make 'Parsec.ParsecT' an instance of 'Parsing'.
instance Parsec.Stream s m Char => Parsing (Parsec.ParsecT s u m) where
  try         = Parsec.try
  satisfy     = Parsec.satisfy
  char        = Parsec.char
  notChar c   = Parsec.satisfy (/= c)
  anyChar     = Parsec.anyChar
  string      = Parsec.string
  space       = Parsec.space
  endOfLine   = Parsec.endOfLine
  manyTill    = Parsec.manyTill 
  (<?>)       = (Parsec.<?>)
  digit       = Parsec.digit 
  many1       = Parsec.many1 
  noneOf      = Parsec.noneOf 
  option      = Parsec.option 
  unexpected  = Parsec.unexpected 

-- | run a 'Parser' computation using Parsec.
--
-- @parse p filename s@ runs parser @p@ on the string @s@,
-- obtained from source @filePath@.
-- The @filePath@ is only used in error messages and may be the empty
-- string. Returns either an error message, a 'String' ('Left') or a
-- value of type @a@ ('Right').
parse
  :: Parser a -> String -> String -> Either String a
parse p filename s = 
  case Parsec.parse p filename s of
    Left err -> Left $ show err
    Right res -> Right res



