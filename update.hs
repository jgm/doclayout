#!/usr/bin/env stack
-- stack --resolver lts-18.10 script --package megaparsec --package bytestring --package pretty-show --package pretty --package text
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as B
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (hang, render, text)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (hexadecimal)


type Parser = Parsec () Text

data Unicode
    = UnicodeChar Char UnicodeWidth Text
    | UnicodeRange Char Char UnicodeWidth Text
    deriving (Show)

data UnicodeWidth = Narrow | Wide | Combining | Control | Ambiguous
  deriving (Show, Eq)

-- | Collapse unicode character ranges if the general category doesn't make a
-- difference for width.
mergeUnicodeCategories :: [Unicode] -> [(Char, UnicodeWidth)]
mergeUnicodeCategories []       = []
mergeUnicodeCategories [x]      = [(unicodeRangeStart x, unicodeWidth x)]
mergeUnicodeCategories (x:y:xs)
    | xw == yw  = mergeUnicodeCategories (x:xs)
    | otherwise = (unicodeRangeStart x, xw) : mergeUnicodeCategories (y:xs)
  where
    xw = unicodeWidth x
    yw = unicodeWidth y

unicodeRangeStart :: Unicode -> Char
unicodeRangeStart (UnicodeChar start _ _)    = start
unicodeRangeStart (UnicodeRange start _ _ _) = start

-- | Get the width from the East Asian Width class and the general category
unicodeWidth :: Unicode -> UnicodeWidth
unicodeWidth (UnicodeChar _ w cat)    = fromMaybe w $ processCat cat
unicodeWidth (UnicodeRange _ _ w cat) = fromMaybe w $ processCat cat

processCat :: Text -> Maybe UnicodeWidth
processCat "Mn" = Just Combining
processCat "Me" = Just Combining
processCat "Mc" = Nothing         -- Spacing marks, often used in abugidas, are combining but also add to the width
processCat "Cc" = Just Control
processCat "Cf" = Just Control
processCat _    = Nothing

-- | Parse a Unicode spec file containing lists of valid emoji
unicodeSpecP :: Parser [Unicode]
unicodeSpecP = fmap concat . many $ choice
    [ [] <$ char '#' <* skipTillEol
    , [] <$ eol
    , pure <$> unicodeP <* skipTillEol
    ]

-- | Parse a line representing unicode datapoints
unicodeP :: Parser Unicode
unicodeP = do
    range <- rangeP
    char ';'
    width <- widthP
    _ <- space
    _ <- string "# "
    generalCategory <- takeP (Just "General Category") 2
    return $ case range of
        Left i       -> UnicodeChar i width generalCategory
        Right (i, j) -> UnicodeRange i j width generalCategory


widthP :: Parser UnicodeWidth
widthP = choice
    [ Narrow <$ string "Na"
    , Narrow <$ string "N"
    , Narrow <$ string "H"
    , Wide <$ string "W"
    , Wide <$ string "F"
    , Ambiguous <$ string "A"
    ]

-- | Parse either a single or a range of hexadecimal code points, representing
-- one or many unicode characters.
rangeP :: Parser (Either Char (Char, Char))
rangeP = do
    start <- chr <$> hexadecimal
    end <- optional $ string ".." *> (chr <$> hexadecimal)
    return $ case end of
        Nothing -> Left start
        Just e  -> Right (start, e)

-- | Parse anything until the end of the line
skipTillEol :: Parser ()
skipTillEol = () <$ skipManyTill anySingle eol

-- | Parse a Unicode spec file and error on failure
parseUnicodeFile :: FilePath -> IO [Unicode]
parseUnicodeFile fp = either (error . show) return . runParser unicodeSpecP fp . TL.toStrict . decodeUtf8 =<< B.readFile fp


main :: IO ()
main = do
  unicode <- mergeUnicodeCategories <$> parseUnicodeFile "EastAsianWidth.txt"
  putStrLn . render $
      hang (text "unicodeSpec =") 2 (ppDoc unicode)
