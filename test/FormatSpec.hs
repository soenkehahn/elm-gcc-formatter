
module FormatSpec where

import Test.Hspec
import System.Exit

import Format

spec :: Spec
spec = do
  describe "format" $ do
    let expected = unlines $
          "" :
          "./src/Levels.elm:20:12: NAMING ERROR" :
          "" :
          "  overview text 1" :
          "" :
          "  details text 1" :
          []
        expected2 = unlines $
          "" :
          "././LevelsTest.elm:24:33: TYPE MISMATCH" :
          "" :
          "  overview text 2" :
          "" :
          "  details text 2" :
          []
    it "converts elm's json output to gcc format" $ do
      errorText <- readFile "test/elm-error.txt"
      let (output, _) = format Nothing errorText
      output `shouldBe` expected

    it "tries to parse the input line by line" $ do
      file1 <- readFile "test/elm-error.txt"
      let middle = "successful compilation, bla bla"
      file2 <- readFile "test/elm-error-2.txt"
      let errorText = unlines $
            file1 :
            middle :
            file2 :
            []
      let (output, _) = format Nothing errorText
      output `shouldBe` (unlines $
        expected :
        middle :
        expected2 :
        [])

    context "when input cannot be parsed" $ do
      it "pipes input through" $ do
        let errorText = "invalid json\n"
        let (output, _) = format Nothing errorText
        output `shouldBe` errorText

    it "allows to specify a parent directory for found files" $ do
      errorText <- readFile "test/elm-error.txt"
      let (output, _) = format (Just "parent") errorText
      (lines output !! 1) `shouldBe`
        "./parent/src/Levels.elm:20:12: NAMING ERROR"

    describe "exit codes" $ do
      it "returns a zero exit code if there's no errors" $ do
        let errorText = "[]"
        let (_, exitCode) = format (Just "parent") errorText
        exitCode `shouldBe` ExitSuccess

      it "returns with a non-zero exit code if there's errors" $ do
        errorText <- readFile "test/elm-error.txt"
        let (_, exitCode) = format (Just "parent") errorText
        exitCode `shouldBe` ExitFailure 1
