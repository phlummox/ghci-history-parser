-- vim: ts=2 sw=2 et :

{-# LANGUAGE NoMonomorphismRestriction, RankNTypes #-}

{- | Some minimal bits of E Kmett's parser combinator library,
  <https://github.com/ekmett/parsers>.
-}

module Parsing (
  module Parsing,
  module Control.Applicative
)
where


import Control.Applicative
import Control.Monad

-- | Generic Parsing type-class, based on a class
-- from E Kmett's parser combinator
-- library, <https://github.com/ekmett/parsers>
class Alternative m => Parsing m where
  -- | try something, then backtrack if it doesn't consume anything
  try :: m a -> m a
  -- | Give a parser a name. Won't necessarily do anything
  -- in all implementations. (But works in Parsec.)
  (<?>) :: m a -> String -> m a
 
  -- | A version of 'many' that discards its input.
  skipMany :: m a -> m ()
  skipMany p = () <$ many p
 
  -- | @skipSome p@ will apply parser @p@ one or more times, skipping
  -- its result. (aka skipMany1 in parsec)
  skipSome :: m a -> m ()
  skipSome p = p *> skipMany p

  -- | emit an error on an unexpected token
  unexpected :: String -> m a
 
  -- | @notFollowedBy p@: succeeds when parser @p@ fails. Consumes
  -- no input. 
  notFollowedBy :: (Monad m, Show a) => m a -> m ()
  notFollowedBy p = try ((try p >>= unexpected . show) <|> pure ())

  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (Char -> Bool) -> m Char

  -- | parses a character, return it
  char :: Char -> m Char
  char c = satisfy (c ==) <?> show [c]

  -- | succeeds for any character
  anyChar :: m Char
  anyChar = satisfy (const True)

  -- | parse any character other than some character. 
  notChar :: Char -> m Char
  notChar c = satisfy (c /=)

  -- | succeeds at end of the input. Defined using 'notFollowedBy'.
  eof :: Monad m => m ()
  eof  = notFollowedBy anyChar <?> "end of input"
 
  -- | parse a string 
  string :: String -> m String
  string s = s <$ try (traverse_ char s) <?> show s
    where
      -- traverse something, but ignore the result
      traverse_ f = foldr ((*>) . f) (pure ())

  -- | parse a digit
  digit :: m Char

  -- | parse one or more of some other parser
  many1 :: m a -> m [a]

  -- | satisfied by any char which is not in the string
  noneOf :: String -> m Char

  -- | parses a space character
  space :: m Char

  -- | something optional; if it doesn't parse, return the first arg
  option :: a -> m a -> m a

  -- | read many of some parser until an end parser is satisfied
  manyTill:: m a -> m end -> m [a]

  -- | parse an end-of-line sequence (newline or crlf)
  endOfLine :: m Char


-- | a monadic Parsing computation
type Parser a = (forall m . (Monad m, Parsing m, Alternative m) => m a) 


