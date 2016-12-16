# ghci-history-parser [![Hackage version](https://img.shields.io/hackage/v/ghci-history-parser.svg?label=Hackage)](https://hackage.haskell.org/package/ghci-history-parser) [![Linux Build Status](https://img.shields.io/travis/phlummox/ghci-history-parser.svg?label=Linux%20build)](https://travis-ci.org/phlummox/ghci-history-parser)

## Parse the output of ghci's ":history" command

I couldn't see a simple parser for the output of the ghci `:history`
command, with few dependencies, so here is one.

If Parsec is installed, this parser will use it, but if not, it will fall
back on the parsers in `Text.ParserCombinators.ReadP`, found in `base`.

For an example of use, see the documentation for the module `GHCi.History.Parse`.
