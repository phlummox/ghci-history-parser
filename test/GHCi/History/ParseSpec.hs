-- vim: ts=2 sw=2 et :

module GHCi.History.ParseSpec 

where

import Test.Hspec
import GHCi.History
import GHCi.History.Parse ( parseHistory )

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

left :: Either a b -> a
left (Left a) = a
left _ = error "not a left"



-- parseHistory :: String -> Either String [HistoryItem]

emptyHistoryStr = "Empty history. yibbideh yibbideh"

nonEmptyHistoryStrA = unlines [
    "-1  : fib (src/Stuff.hs:52:8-16)"
   , "<end of history>"
  ]

nonEmptyHistoryA = [
  HistoryItem (-1) "fib" "src/Stuff.hs" (FilePos 52 8) (FilePos 52 16)
  ]

nonEmptyHistoryStrB = unlines [
    "-1  : fib (src/Stuff.hs:52:8-16)"
  , "-2  : fib (src/Stuff.hs:(51,9)-(54,16))"
  , "-3  : fib (src/Stuff.hs:(46,1)-(54,16))"
   , "<end of history>"
  ]

nonEmptyHistoryB = [
  HistoryItem (-1) "fib" "src/Stuff.hs" (FilePos 52 8) (FilePos 52 16)
  ,HistoryItem (-2) "fib" "src/Stuff.hs" (FilePos 51 9) (FilePos 54 16)
  ,HistoryItem (-3) "fib" "src/Stuff.hs" (FilePos 46 1) (FilePos 54 16)
  ]

nonEmptyHistoryStrC = unlines [
    "-1 : fib (src/Stuff.hs:10:13-15)"
  , "-2 : fib (src/Stuff.hs:(4,1)-(12,16))"
  , "..."
  ]

nonEmptyHistoryC = [
  HistoryItem (-1) "fib" "src/Stuff.hs" (FilePos 10 13) (FilePos 10 15)
  ,HistoryItem (-2) "fib" "src/Stuff.hs" (FilePos 4 1) (FilePos 12 16)
  ]

main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "parseHistory" $ do
    it "it parses empty history to an empty list" $ do
      parseHistory emptyHistoryStr `shouldBe` (Right [])

  describe "parseHistory" $ do
    it "it parses non-empty history to an expected list" $ do
      parseHistory nonEmptyHistoryStrA `shouldBe` (Right nonEmptyHistoryA)

  describe "parseHistory" $ do
    it "also more non-empty history to another expected list" $ do
      parseHistory nonEmptyHistoryStrB `shouldBe` (Right nonEmptyHistoryB)

  describe "parseHistory" $ do
    it "also should handle ellipses at end for long histories" $ do
      parseHistory nonEmptyHistoryStrC `shouldBe` (Right nonEmptyHistoryC)

  describe "parseHistory" $ do
    it "should give some sort of error for badly formed things" $ do
      parseHistory "XXX" `shouldSatisfy` 
        \x -> isLeft x && (length (left x) > 0)


  describe "parseHistory" $ do
    it "should successfully parse 'point ranges'" $ do
      let
        pointRangeStr = unlines 
          ["-1  : fib (fib.hs:3:10)"
          ,"-2  : fib:a (fib.hs:6:14-16)"
          ,"<end of history>"]
        pointRangeExpected = [
          HistoryItem (-1) "fib" "fib.hs" (FilePos 3 10) (FilePos 3 10)
          ,HistoryItem (-2) "fib:a" "fib.hs" (FilePos 6 14) (FilePos 6 16)
          ]
      parseHistory pointRangeStr `shouldBe` (Right pointRangeExpected) 





