{-# LANGUAGE OverloadedStrings #-}
import Text.DocLayout
import Criterion.Main
import Criterion.Types (Config (..))

main :: IO ()
main = defaultMainWith defaultConfig{ timeLimit = 10.0 } $ cases

cases :: [Benchmark]
cases = [
  bench "soft spaces at end of line" $
    nf (render Nothing) ("a" <> mconcat (replicate 50 (space <> box 1 mempty)))
    ]
