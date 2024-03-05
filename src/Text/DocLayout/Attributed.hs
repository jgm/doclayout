{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.DocLayout.Attributed (Attributed(..))
  where

import Data.String
import Text.DocLayout.ANSIFont (Font, baseFont)
import Data.Data (Data, Typeable)
import GHC.Generics

data Attributed a = Attr Font a
                  | Concattr (Attributed a) (Attributed a)
                  | Null
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable,
          Data, Typeable, Generic)

instance Semigroup a => Semigroup (Attributed a) where
  (<>) a@(Attr f1 x1) b@(Attr f2 x2)
    | f1 == f2  = Attr f1 (x1 <> x2)
    | otherwise = Concattr a b
  (<>) (Concattr a b) c = Concattr a (b <> c)
  (<>) a@(Attr _ _) (Concattr b c) = Concattr (a <> b) c
  (<>) Null b = b
  (<>) a Null = a

instance Monoid a => Monoid (Attributed a) where
  mempty = Null

instance IsString a => IsString (Attributed a) where
  fromString x | null x = Null
               | otherwise = Attr baseFont (fromString x)
