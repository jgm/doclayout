{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.DocLayout
import Text.Emoji

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Criterion.Main
import Criterion.Types (Config (..))
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

main :: IO ()
main = do
    udhrEng <- udhrLang "eng"
    udhrFrn <- udhrLang "frn"
    udhrVie <- udhrLang "vie"
    udhrChn <- udhrLang "chn"
    udhrArz <- udhrLang "arz"
    udhrHnd <- udhrLang "hnd"
    udhrBng <- udhrLang "bng"
    udhrRus <- udhrLang "rus"
    udhrJpn <- udhrLang "jpn"
    udhrKkn <- udhrLang "kkn"
    udhrTcw <- udhrLang "tcw"
    udhrTcv <- udhrLang "tcv"
    udhrThj <- udhrLang "thj"
    udhrGrk <- udhrLang "grk"
    emojiTxt <- evaluate . force . T.replicate 1000 $ mconcat baseEmojis <> mconcat zwjEmojis
    defaultMainWith defaultConfig{ timeLimit = 10.0 }
      [ bench "sample document 2" $
          nf (render Nothing :: Doc Text -> Text)
             (nest 3 $ cblock 20 $ vcat $ replicate 15 $
               hsep $ map text $ words bigtext)

      , bench "reflow English" $
          nf (render (Just 20) :: Doc Text -> Text) $ flowedDoc udhrEng

      , bench "reflow Greek" $
          nf (render (Just 20) :: Doc Text -> Text) $ flowedDoc udhrGrk

      , bench "tabular English" $
          nf (render (Just 80) :: Doc Text -> Text)
             (let blah = hsep . map literal . take 800 . T.words $ udhrEng
              in  cblock 20 blah <> lblock 30 blah <> rblock 10 blah $$
                  cblock 50 (nest 5 blah) <> rblock 10 blah)

      , bench "tabular Greek" $
          nf (render (Just 80) :: Doc Text -> Text)
             (let blah = hsep . map literal . take 800 . T.words $ udhrGrk
              in  cblock 20 blah <> lblock 30 blah <> rblock 10 blah $$
                  cblock 50 (nest 5 blah) <> rblock 10 blah)

      , bench "soft spaces at end of line" $
          nf (render Nothing :: Doc Text -> Text)
             ("a" <> mconcat (replicate 50 (space <> lblock 1 mempty)))

      -- Benchmarks for languages using all scripts used by more than 50 million people
      -- https://en.wikipedia.org/wiki/List_of_writing_systems#List_of_writing_systems_by_adoption
      -- https://www.unicode.org/udhr/translations.html
      [ bench "UDHR English"    $ nf realLength udhrEng  -- Plain ASCII
      , bench "UDHR French"     $ nf realLength udhrFrn  -- Latin with some diacritics
      , bench "UDHR Vietnamese" $ nf realLength udhrVie  -- Latin with more diacritics
      , bench "UDHR Mandarin"   $ nf realLength udhrChn  -- Mandarin
      , bench "UDHR Arabic"     $ nf realLength udhrArz  -- Arabic
      , bench "UDHR Hindi"      $ nf realLength udhrHnd  -- Hindi
      , bench "UDHR Bengali"    $ nf realLength udhrBng  -- Bengali
      , bench "UDHR Russian"    $ nf realLength udhrRus  -- Russian
      , bench "UDHR Japanese"   $ nf realLength udhrJpn  -- Japanese
      , bench "UDHR Korean"     $ nf realLength udhrKkn  -- Korean
      , bench "UDHR Telugu"     $ nf realLength udhrTcw  -- Telugu
      , bench "UDHR Tamil"      $ nf realLength udhrTcv  -- Tamil
      -- Benchmarks for other languages
      , bench "UDHR Thai"       $ nf realLength udhrThj  -- Thai
      , bench "UDHR Greek"      $ nf realLength udhrGrk  -- Greek
      , bench "Emoji"           $ nf realLength emojiTxt -- Emoji
      ]

-- | The Universal declaration of human rights in a given language, repeated 1000 times.
udhrLang :: String -> IO Text
udhrLang lang = do
    txt <- T.readFile ("udhr/txt/" ++ lang ++ ".txt")
    evaluate . force $ T.replicate 1000 txt

bigtext :: String
bigtext = "Hello there. This is a big text."

flowedDoc :: Text -> Doc Text
flowedDoc txt = hsep $ map literal . take 800 . T.words $ txt

