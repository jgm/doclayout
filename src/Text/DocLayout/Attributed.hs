{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.DocLayout.Attributed (Attributed(..), Attr(..), Link, fromList, singleton)
  where

import Data.String
import Text.DocLayout.ANSIFont (Font, baseFont)
import Data.Data (Data, Typeable)
import GHC.Generics
import Data.Sequence ((><))
import qualified Data.Sequence as S
import Data.Text (Text)

type Link = Maybe Text

data Attr a = Attr Link Font a
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable,
    Data, Typeable, Generic)

instance Semigroup a => Semigroup (Attr a) where
  (<>) (Attr l f x) (Attr _ _ y) = Attr l f $ x <> y  -- This is arbitrary

instance (IsString a, Monoid a) => Monoid (Attr a) where
  mempty = Attr Nothing baseFont (fromString "")

newtype Attributed a = Attributed (S.Seq (Attr a))
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable,
         Data, Typeable, Generic)

fromList :: [Attr a] -> Attributed a
fromList = Attributed . S.fromList

singleton :: Attr a -> Attributed a
singleton = Attributed . S.singleton

instance IsString a => IsString (Attr a) where
  fromString x = Attr Nothing baseFont (fromString x)

instance IsString a => IsString (Attributed a) where
  fromString x = Attributed $ S.singleton $ Attr Nothing baseFont (fromString x)

instance Semigroup a => Semigroup (Attributed a) where
  (<>) (Attributed a) (Attributed b) = Attributed $ a >< b

instance Monoid a => Monoid (Attributed a) where
  mempty = Attributed S.empty
