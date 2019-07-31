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
  [ testCase "simple text with truncation" $
      render (Just 4) ("hello" <+> "there")
      @?= ("hell\nther" :: String)

  , testCase "nontrivial empty doc" $
      isEmpty (nest 5 (alignCenter empty))
      @?= True

  , testCase "nontrivial nonempty doc" $
      isEmpty (box 1 (text "a"))
      @?= False

  , testCase "prefixed with multi paragraphs" $
      render (Just 80) (prefixed ">" ("foo" <> cr <> "bar" <> blankline <> "baz"))
      @?= (">foo\n>bar\n>\n>baz" :: String)

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
  ]
