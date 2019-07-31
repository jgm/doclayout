{-# LANGUAGE OverloadedStrings #-}
import Text.DocLayout
import Data.Text (Text)
import Criterion.Main
import Criterion.Types (Config (..))
import qualified Text.Pandoc.Pretty as P

main :: IO ()
main = defaultMainWith defaultConfig{ timeLimit = 10.0 } $ cases

bigtext :: String
bigtext = "Hello there. This is a big text."

cases :: [Benchmark]
cases =
  [ bench "sample document" $
      nf (render Nothing)
         (nest 3 $ prefixed "> " $ vcat $ replicate 15 $
           hsep $ map text $ words bigtext)
  , bench "sample document (Text.Pandoc.Pretty)" $
      nf (P.render Nothing :: P.Doc -> Text)
         (P.nest 3 $ P.prefixed "> " $ P.vcat $ replicate 15 $
           P.hsep $ map P.text $ words bigtext)
  , bench "soft spaces at end of line" $
      nf (render Nothing)
         ("a" <> mconcat (replicate 50 (space <> box 1 mempty)))
  ]
