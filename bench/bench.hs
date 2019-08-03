{-# LANGUAGE OverloadedStrings #-}
import Text.DocLayout
import Data.Text (Text)
import Data.String.Conversions
import Criterion.Main
import Criterion.Types (Config (..))

main :: IO ()
main = defaultMainWith defaultConfig{ timeLimit = 10.0 } $ cases

bigtext :: String
bigtext = "Hello there. This is a big text."

cases :: [Benchmark]
cases =
  [ bench "sample document 1" $
      nf (render Nothing :: Doc -> LazyText)
         (nest 3 $ prefixed "> " $ vcat $ replicate 15 $
           hsep $ map text $ words bigtext)

  , bench "sample document 2" $
      nf (render Nothing :: Doc -> LazyText)
         (nest 3 $ cblock 20 $ vcat $ replicate 15 $
           hsep $ map text $ words bigtext)

  , bench "reflow" $
      nf (render (Just 20) :: Doc -> LazyText)
         (hsep $ map text $ words . unwords $ replicate 50 bigtext)

  , bench "tabular" $
      nf (render (Just 80) :: Doc -> LazyText)
         (let blah = hsep $ map text $ words . unwords
                           $ replicate 50 bigtext
          in  cblock 20 blah <> lblock 30 blah <> rblock 10 blah $$
              cblock 50 (nest 5 blah) <> rblock 10 blah)

  , bench "soft spaces at end of line" $
      nf (render Nothing :: Doc -> LazyText)
         ("a" <> mconcat (replicate 50 (space <> box 1 mempty)))
  ]
