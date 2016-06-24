-- vim: ts=2 sw=2 et :

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts  #-}

{-# LANGUAGE RankNTypes  #-}

module Parsing.Parsec where


--import GHCi.History (
--    HistoryItem(..)
--  , FilePos(..) 
--  )

import Parsing
import qualified Text.Parsec as Parsec
--import qualified Text.Parsec as Parsec
import Text.Parsec.Error (
    messageString
  , errorPos
  , errorMessages 
  )

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

parse
  :: Parser a -> String -> String -> Either String a
parse p filename s = 
  case Parsec.parse p filename s of
    Left err -> Left $ show err
    Right res -> Right res



