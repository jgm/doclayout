{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.DocLayout
import Text.Emoji
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

main :: IO ()
main = defaultMain $ localOption (mkTimeout (480 * 10^6)) $ testGroup "Tests" tests
-- 8 minute timeout

renderTest :: String -> Maybe Int -> Doc Text -> Text -> TestTree
renderTest title mblen doc expected =
  testCase title $ render mblen doc @?= expected

tests :: [TestTree]
tests =
  [ testCase "offset prefixed" $
      (offset (nest 3 (text "**" <> text "thisIsGoingToBeTooLongAnyway" <>
                       text "**") <> blankline :: Doc Text) @?= 35)

  , renderTest "lblock with chop"
      Nothing
      (lblock 4 (text "hi there" :: Doc Text))
      "hi t\nhere"

  , renderTest "nesting should not wrap"
      (Just 10)
      (nest 3 (text "abcdefghi" :: Doc Text))
      "   abcdefghi"

  , renderTest "simple vcat"
      (Just 10)
      (vcat $ map chomp ["aaa", "bbb", "ccc"])
      "aaa\nbbb\nccc"

  , renderTest "vcat with chomp"
      (Just 10)
      (chomp "aaa" $$ chomp "bbb" $$ chomp "ccc")
      "aaa\nbbb\nccc"

  , renderTest "simple text above line length"
      (Just 4)
      ("hello" <+> "there")
      "hello\nthere"

  , renderTest "cr"
      (Just 60)
      ("hello" <> cr <> "there")
      "hello\nthere"

  , renderTest "x $$ cr"
      Nothing
      ("x" $$ cr)
      "x\n"

 , renderTest "wrapping"
     (Just 10)
     (hsep ["hello", "there", "this", "is", "a", "test"])
     "hello\nthere this\nis a test"

 , renderTest "simple box wrapping"
     (Just 50)
     (lblock 3 "aa" <> lblock 3 "bb" <> lblock 3 ("aa" <+> "bbbb"))
     "aa bb aa\n      b\n      bbb"

 , renderTest "prefixed with multi paragraphs"
     (Just 80)
     (prefixed "> " ("foo" <+> "bar" <> cr <>
              "baz" <> blankline <> "bim" <+> "bam"))
     "> foo bar\n> baz\n>\n> bim bam"

 , renderTest "prefixed with hsep"
     Nothing
     (prefixed "> " $ hsep ["a","b","c"])
     "> a b c"

 , renderTest "nest"
     Nothing
     (nest 4 "aa\n\nbb\ncc")
     "    aa\n\n    bb\n    cc"

 , renderTest "hang"
     Nothing
     (hang 4 "  - " (chomp "aa\n\nbb\ncc" <> cr) <>
                     hang 4 "  - " (chomp "dd\n" <> cr))
     "  - aa\n\n    bb\n    cc\n  - dd\n"

 , renderTest "align with box"
     Nothing
     ("aa" <> lblock 2 ("bb" $$ "cc") <> "dd")
     "aabb\n  ccdd"

 , renderTest "centered box"
     Nothing
     ("aa" <> cblock 4 ("bb" $$ "cc"))
     "aa bb\n   cc"

 , renderTest "blanks at beginning"
     Nothing
     (blanklines 2 <> "aa")
     "\naa"  -- only one because we treat top of doc as implicit blank

 , renderTest "blanks at end"
     Nothing
     ("aa" <> blanklines 2)
     "aa\n"

 , renderTest "blanks at end with multiple"
     Nothing
     ("aa" <> cr <> blanklines 2 <> blanklines 0)
     "aa\n"

 , renderTest "blanks at end with nesting"
     Nothing
     (nest 2 (nest 3 ("aa" <> blankline) <> cr <> blanklines 2) <> blanklines 2)
     "     aa\n"

 , renderTest "blanks around cr"
     Nothing
     ("aa" <> blankline <> cr <> blankline <> "bb")
     "aa\n\nbb"

  , renderTest "strange wrap case"
     (Just 8)
     (vcat [hang 2 "- " (chomp $ text "aaaaa" <> space <> "bbb"), hang 2 "- " (text "aoeu")])
     "- aaaaa\n  bbb\n- aoeu"

  , renderTest "strange wrap case"
     (Just 8)
     (text "aaaaa" <> space <> text "bbbbb" <> blankline <> text "ccccc")
     "aaaaa\nbbbbb\n\nccccc"

 , renderTest "chomp 1"
     Nothing
     (chomp (("aa" <> space) <> blankline) <> "bb")
     "aabb"

 , renderTest "chomp 2"
     Nothing
     (chomp ("aa" <> space) <> "bb")
     "aabb"

 , renderTest "chomp 3"
     Nothing
     (chomp "aa")
     "aa"

 , renderTest "chomp with nesting"
     Nothing
     (chomp (nest 3 ("aa" <> blankline)))
     "   aa"

 , renderTest "chomp with box at end"
     Nothing
     ("aa" <> cr <> chomp (lblock 2 ("aa" <> blankline) <> blankline))
     "aa\naa"

 , renderTest "empty and $$"
     Nothing
     ("aa" $$ empty $$ "bb")
     "aa\nbb"

 , renderTest "table"
     Nothing
     ((rblock 4 "aa" <> lblock 3 " | " <> cblock 4 "bb" <>
                         lblock 3 " | " <> lblock 4 "cc") $$
                     (rblock 4 "----" <> lblock 3 " | " <> cblock 4 "----" <>
                         lblock 3 " | " <> lblock 4 "----") $$
                     (rblock 4 "dd" <> lblock 3 " | " <> cblock 4 "ee" <>
                         lblock 3 " | " <> lblock 4 "ff"))
     "  aa |  bb  | cc\n---- | ---- | ----\n  dd |  ee  | ff"

 , renderTest "proper wrapping with multiple components"
     (Just 10)
     ("aa" <> space <> "bbbbb" <> "ccccc")
     "aa\nbbbbbccccc"

 , renderTest "nested wrapped text"
    (Just 10)
    (nest 5 (hsep ["hi", "there", "my", "friend"]) <> cr)
    "     hi\n     there\n     my\n     friend\n"

 , renderTest "aligned wrapped text"
    Nothing
    (cblock 7 ("hi" <+> "there"))
    "  hi\n there"

  , renderTest "afterBreak"
      (Just 2)
      ("hi" <+> afterBreak "!" <> afterBreak "?" <> "x" <> afterBreak "?")
      "hi\n!x"

  , renderTest "breaks and nest"
      (Just 5)
      ("[" <> nest 1 ("aaaaaaaaa" $$ "bbbbbbb") <> "]")
      "[aaaaaaaaa\n bbbbbbb]"

  , renderTest "empty nest"
      Nothing
      ("aa" $$ nest 3 mempty $$ "bb")
      "aa\nbb"

  , renderTest "hsep with empty"
      Nothing
      (hsep ["a",mempty,"b"])
      "a b"

   , renderTest "(<+>) with empty"
      Nothing
      ("a" <+> mempty <+> "b")
      "a b"

  , renderTest "vcat doesn't create newline at end"
      Nothing
      (vcat ["aa","bb"] <> "cc")
      "aa\nbbcc"

  , renderTest "vcat []"
      Nothing
      (vcat [])
      ""

  , renderTest "nestle"
      Nothing
      (nestle $ blanklines 2 $$ "aa" $$ blanklines 2 <> cr)
      "aa\n"

  , renderTest "prefix with box"
      Nothing
      (prefixed "> " $ cblock 4 ("aa" $$ "bb"))
      ">  aa\n>  bb"

  , renderTest "breaking space after empty"
      Nothing
      (empty <> space <> "x")
      "x"

  , renderTest "hang with empty content"
      Nothing
      (hang 5 (nowrap "xxx") mempty)
      "xxx"

  , renderTest "beforeNonBlank"
      Nothing
      (beforeNonBlank "!!" <> " ab" $$
       beforeNonBlank "!!" <> "a b")
      " ab\n!!a b"

  , renderTest "vfill"
      Nothing
      (vfill "|" <> cblock 5 ("a" $$ "bbb" $$ "ccccc") <> lblock 3 "dd" <>
          vfill "+")
      "|  a  dd +\n| bbb    +\n|ccccc   +"

  , renderTest "vfill 2"
      Nothing
      (vfill "| " <> cblock 5 ("a" $$ "bbb") <> vfill " | " <>
          lblock 2 ("dd" $$ "ee" $$ "ff") <> vfill " |")
      "|   a   | dd |\n|  bbb  | ee |\n|       | ff |"

  , renderTest "combining character at start"
      Nothing
      (text "\870" <> space <> text "a")
      "\870 a"

  , renderTest "newline after cr"  -- #20
      Nothing
      (literal "a" <> cr <> literal "\nb" <> cr <> literal "c")
      "a\n\nb\nc"

  , testCase "length of normal text" $
      realLength ("This is going to be too long anyway" :: String) @?= 35

  , testCase "length of an ambiguous character in normal context" $
      realLength ("Circle\x25EF\&Circle" :: String) @?= 13

  , testCase "length of an ambiguous character in wide context" $
      realLengthWideContext ("圆\x25EF\&圆" :: String) @?= 6

  , testCase "length of normal character, which could be continued to an emoji, but isn't" $
      realLength ("*a" :: String) @?= 2

  , testCase "length of normal character, which could be continued to an emoji, and is" $
      realLength ("*\xFE0F\x20E3\&a" :: String) @?= 3

  , testCase "length emoji consisting of one code point" $
      realLength ("\x231A" :: String) @?= 2

  , testCase "length of an emoji constructed using the variating modifier" $
      realLength ("\x00A9\xFE0F" :: String) @?= 2

  , testCase "length of a non-emoji which would be an emoji with a variation modifier" $
      realLength ("\x00A9" :: String) @?= 1

  , testCase "length of two emoji in a row" $
      realLength ("\x1F170\xFE0F\x1F1E6\x1F1E8" :: String) @?= 4

  , testCase "length of an emoji with skin tone modifier, where stripping results in a non-emoji" $
      realLength ("\x1F590\x1F3FF" :: String) @?= 2

  , testCase "a digit with a skin tone modifier is invalid but might appear, and shouldn't be mistaken for a variation modifier" $
      realLength ("1\x1F3FF" :: String) @?= 3

  , testGroup "all base emoji have width 2" $
      baseEmojis <&> \emoji -> testCase (T.unpack emoji) $ realLength emoji @?= 2

  , testGroup "all zero-width joiner emoji sequences have width 2" $
      zwjEmojis <&> \emoji -> testCase (T.unpack emoji) $ realLength emoji @?= 2

  , testProperty "shortcut provides same answer for string length in a narrow context" . withMaxSuccess 10000 $

      \(x :: String) -> realLengthNarrowContext x === realLengthNarrowContextNoShortcut x

  , testProperty "shortcut provides same answer for string length in a wide context" . withMaxSuccess 10000 $
      \(x :: String) -> realLengthWideContext x === realLengthWideContextNoShortcut x
  ]
