{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.DocLayout
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)

main :: IO ()
main = defaultMain $ testGroup "Tests" tests

tests :: [TestTree]
tests =
  [ testCase "simple text above line length" $
      render (Just 4) ("hello" <+> "there")
      @?= ("hello\nthere" :: String)

  , testCase "cr" $
      render (Just 60) ("hello" <> cr <> "there")
      @?= ("hello\nthere" :: String)

  , testCase "wrapping" $
      render (Just 10) (hsep ["hello", "there", "this", "is", "a", "test"])
      @?= ("hello\nthere this\nis a test" :: String)

  , testCase "simple box wrapping" $
      render (Just 50) (box 3 "aa" <> box 3 "bb" <> box 3 ("aa" <+> "bbbb"))
      @?= ("aa bb aa\n      bbbb" :: Text)

  , testCase "nontrivial empty doc" $
      isEmpty (nest 5 (alignCenter empty))
      @?= True

  , testCase "nontrivial nonempty doc" $
      isEmpty (box 1 (text "a"))
      @?= False

  , testCase "prefixed with multi paragraphs" $
      render (Just 80) (prefixed "> " ("foo" <> cr <> "bar" <> blankline <> "baz"))
      @?= ("> foo\n> bar\n>\n> baz" :: String)

  , testCase "breaking space before empty box" $
      render Nothing ("a" <> space <> box 3 mempty)
      @?= ("a" :: String)

  , testCase "centered" $
      render (Just 10) (alignCenter "hi\nlo")
      @?= ("    hi\n    lo" :: String)

  , testCase "vfill" $
      render Nothing (vfill "|" <> box 2 (vcat $ replicate 4 "aa") <>
                      vfill "|")
      @?= ("|aa|\n|aa|\n|aa|\n|aa|" :: Text)

  , testCase "aligned" $
      render Nothing ("aa" <> aligned ("bb" $$ "cc") <> "dd")
      @?= ("aabb\n  ccdd" :: Text)

  , testCase "align with box" $
      render Nothing ("aa" <> box 2 ("bb" $$ "cc") <> "dd")
      @?= ("aabbdd\n  cc" :: Text)

  , testCase "centered box" $
      render Nothing ("aa" <> box 4 (alignCenter $ "bb" $$ "cc") <> "dd")
      @?= ("aa bb dd\n   cc" :: Text)

  , testCase "blanks at beginning" $
      render Nothing (blanklines 2 <> "aa")
      @?= ("aa" :: Text)

  , testCase "blanks at end" $
      render Nothing ("aa" <> blanklines 2)
      @?= ("aa" :: Text)

  , testCase "chomp with box at end" $
      render Nothing ("aa" <> cr <> chomp (box 2 ("aa" <> blankline) <> blankline))
      @?= ("aa\naa" :: Text)

  , testCase "empty and $$" $
      render Nothing ("aa" $$ empty $$ "bb")
      @?= ("aa\nbb" :: Text)
  ]
