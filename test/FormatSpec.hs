
module FormatSpec where

import Test.Hspec

import Format

spec :: Spec
spec = do
  describe "format" $ do
    let expected = (Right $ unlines $
          "./src/Levels.elm:20:12: NAMING ERROR" :
          "  Cannot find variable `Dict.fromLst`." :
          "" :
          "  `Dict` does not expose `fromLst`. Maybe you want one of the following?\n\n    Dict.fromList" :
          [])
    it "converts elm's json output to gcc format" $ do
      errorText <- readFile "test/elm-error.txt"
      format errorText `shouldBe` expected

    it "tries to parse the output without the last line" $ do
      file <- readFile "test/elm-error.txt"
      let errorText = file ++ "\nsuccessful compilation, bla bla\n"
      format errorText `shouldBe` expected
