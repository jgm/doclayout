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
  [
    testCase "simple vcat " $
      render (Just 10) (vcat $ map chomp ["aaa", "bbb", "ccc"])
      @?= ("aaa\nbbb\nccc" :: String)

  , testCase "vcat with chomp" $
      render (Just 10) (chomp "aaa" $$ chomp "bbb" $$ chomp "ccc")
      @?= ("aaa\nbbb\nccc" :: String)

  , testCase "simple text above line length" $
      render (Just 4) ("hello" <+> "there")
      @?= ("hello\nthere" :: String)

  , testCase "cr" $
      render (Just 60) ("hello" <> cr <> "there")
      @?= ("hello\nthere" :: String)

  , testCase "x $$ cr" $
      render Nothing ("x" $$ cr)
      @?= ("x" :: Text)

 , testCase "wrapping" $
     render (Just 10) (hsep ["hello", "there", "this", "is", "a", "test"])
     @?= ("hello\nthere this\nis a test" :: String)

 , testCase "simple box wrapping" $
     render (Just 50) (box 3 "aa" <> box 3 "bb" <> box 3 ("aa" <+> "bbbb"))
     @?= ("aa bb aa\n      bbb" :: Text)

 , testCase "prefixed with multi paragraphs" $
     render (Just 80) (prefixed "> " ("foo" <+> "bar" <> cr <>
              "baz" <> blankline <> "bim" <+> "bam"))
     @?= ("> foo bar\n> baz\n>\n> bim bam" :: String)

 , testCase "prefixed with hsep" $
     render Nothing (prefixed "> " $ hsep ["a","b","c"])
     @?= ("> a b c" :: Text)

 , testCase "breaking space before empty box" $
     render Nothing ("a" <> space <> box 3 mempty)
     @?= ("a" :: String)

 , testCase "centered" $
     render (Just 10) (alignCenter "hi\nlow")
     @?= ("    hi\n   low" :: String)

 , testCase "nest" $
     render Nothing (nest 4 "aa\n\nbb\ncc")
     @?= ("    aa\n\n    bb\n    cc" :: Text)

 , testCase "hang" $
     render Nothing (hang 4 "  - " (chomp "aa\n\nbb\ncc" <> cr) <>
                     hang 4 "  - " (chomp "dd\n" <> cr))
     @?= ("  - aa\n\n    bb\n    cc\n  - dd" :: Text)

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

 , testCase "blanks at end with multiple" $
     render Nothing ("aa" <> cr <> blanklines 2 <> blanklines 0)
     @?= ("aa" :: Text)

 , testCase "blanks at end with nesting" $
     render Nothing (nest 2 (nest 3 ("aa" <> blankline) <> cr <> blanklines 2) <> blanklines 2)
     @?= ("     aa" :: Text)

 , testCase "blanks at end with alignment" $
     render Nothing (alignLeft ("aa" <> blankline) <> cr <> blankline)
     @?= ("aa" :: Text)

 , testCase "blanks around cr" $
     render Nothing ("aa" <> blankline <> cr <> blankline <> "bb")
     @?= ("aa\n\nbb" :: Text)

  , testCase "strange wrap case" $
     render (Just 8) (vcat [hang 2 "- " (chomp $ text "aaaaa" <> space <> "bbb"), hang 2 "- " (text "aoeu")])
     @?= ("- aaaaa\n  bbb\n- aoeu" :: Text)

  , testCase "strange wrap case" $
     render (Just 8) (text "aaaaa" <> space <> text "bbbbb" <> blankline <> text "ccccc")
     @?= ("aaaaa\nbbbbb\n\nccccc" :: Text)

 , testCase "chomp 1" $
     render Nothing (chomp (("aa" <> space) <> blankline) <> "bb")
     @?= ("aabb" :: Text)

 , testCase "chomp 2" $
     render Nothing (chomp ("aa" <> space) <> "bb")
     @?= ("aabb" :: Text)

 , testCase "chomp 3" $
     render Nothing (chomp "aa")
     @?= ("aa" :: Text)

 , testCase "chomp with nesting" $
     render Nothing (chomp (nest 3 ("aa" <> blankline)) <> "bb")
     @?= ("   aabb" :: Text)

 , testCase "chomp with box at end" $
     render Nothing ("aa" <> cr <> chomp (box 2 ("aa" <> blankline) <> blankline))
     @?= ("aa\naa" :: Text)

 , testCase "empty and $$" $
     render Nothing ("aa" $$ empty $$ "bb")
     @?= ("aa\nbb" :: Text)

 , testCase "table" $
     render Nothing ((rblock 4 "aa" <> lblock 3 " | " <> cblock 4 "bb" <>
                         lblock 3 " | " <> lblock 4 "cc") $$
                     (rblock 4 "----" <> lblock 3 " | " <> cblock 4 "----" <>
                         lblock 3 " | " <> lblock 4 "----") $$
                     (rblock 4 "dd" <> lblock 3 " | " <> cblock 4 "ee" <>
                         lblock 3 " | " <> lblock 4 "ff"))
     @?= ("  aa |  bb  | cc\n---- | ---- | ----\n  dd |  ee  | ff" :: Text)

 , testCase "proper wrapping with multiple components" $
     render (Just 10) ("aa" <> space <> "bbbbb" <> "ccccc")
     @?= ("aa\nbbbbbccccc" :: Text)

 , testCase "getDimensions" $
    let foo = "A baosnetuh snaothsn aoesnth aoesnth aosenth sentuhaoeu"
    in getDimensions Nothing (hsep (map text $ words $ foo) <> cr <> "bar")
    @?= Dimensions (length foo) 2

 , testCase "nested wrapped text" $
    render (Just 10) (nest 5 (hsep ["hi", "there", "my", "friend"]) <> cr)
    @?= ("     hi\n     there\n     my\n     friend" :: Text)

 , testCase "aligned wrapped text" $
    render Nothing (cblock 7 ("hi" <+> "there"))
    @?= ("  hi\n there" :: Text)

  , testCase "afterBreak" $
      render (Just 2) ("hi" <+> afterBreak "!" <> afterBreak "?" <>
          "x" <> afterBreak "?")
    @?= ("hi\n!x" :: Text)

  ]
