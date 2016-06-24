-- vim: ts=2 sw=2 et :

{-# LANGUAGE CPP #-}

{- | 
Parse the output of ghci's @:history@ command.
-}   

module GHCi.History.Parse 

where

import GHCi.History
import qualified Parsing
import qualified GHCi.History.Parse.Common as C

#ifdef USE_PARSEC
import qualified Parsing.Parsec as P
#else
import qualified Parsing.ReadP as P
#endif

-- | Given a string containing the output of the ghci @:history@
-- command, parse it using "Text.Parsec", if available,
-- otherwise "Text.ParserCombinators.ReadP".
--
-- Returns either an error message, or a list of 'HistoryItem's.
parseHistory :: String -> Either String [GHCi.History.HistoryItem]
parseHistory str =
  P.parse C.history "(interactive)" str



