{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.DocLayout
import Data.Text (Text)
import Criterion.Main
import Criterion.Types (Config (..))
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif

main :: IO ()
main = defaultMainWith defaultConfig{ timeLimit = 10.0 } $ cases

bigtext :: String
bigtext = "Hello there. This is a big text."

flowedDoc :: Doc Text
flowedDoc = hsep $ map text $ words . unwords $ replicate 500 bigtext

cases :: [Benchmark]
cases =
  [
   bench "sample document 2" $
      nf (render Nothing :: Doc Text -> Text)
         (nest 3 $ cblock 20 $ vcat $ replicate 15 $
           hsep $ map text $ words bigtext)

  , bench "reflow" $
      nf (render (Just 20) :: Doc Text -> Text) flowedDoc

  , bench "tabular" $
      nf (render (Just 80) :: Doc Text -> Text)
         (let blah = hsep $ map text $ words . unwords
                           $ replicate 50 bigtext
          in  cblock 20 blah <> lblock 30 blah <> rblock 10 blah $$
              cblock 50 (nest 5 blah) <> rblock 10 blah)

  , bench "soft spaces at end of line" $
      nf (render Nothing :: Doc Text -> Text)
         ("a" <> mconcat (replicate 50 (space <> lblock 1 mempty)))
  ]
