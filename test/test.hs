{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.DocLayout
import Test.Tasty.Golden
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.FilePath
import Data.Semigroup ((<>))

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [
    ]
