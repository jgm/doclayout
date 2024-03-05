{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.DocLayout.HasChars (HasChars(..)) where
import Prelude
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Text.DocLayout.Attributed

-- | Class abstracting over various string types that
-- can fold over characters.  Minimal definition is 'foldrChar'
-- and 'foldlChar', but defining the other methods can give better
-- performance.
class (IsString a, Semigroup a, Monoid a, Show a) => HasChars a where
  foldrChar     :: (Char -> b -> b) -> b -> a -> b
  foldlChar     :: (b -> Char -> b) -> b -> a -> b
  replicateChar :: Int -> Char -> a
  replicateChar n c = fromString (replicate n c)
  isNull        :: a -> Bool
  isNull = foldrChar (\_ _ -> False) True
  splitLines    :: a -> [a]
  splitLines s = (fromString firstline : otherlines)
   where
    (firstline, otherlines) = foldrChar go ([],[]) s
    go '\n' (cur,lns) = ([], fromString cur : lns)
    go c    (cur,lns) = (c:cur, lns)

instance HasChars Text where
  foldrChar         = T.foldr
  foldlChar         = T.foldl'
  splitLines        = T.splitOn "\n"
  replicateChar n c = T.replicate n (T.singleton c)
  isNull            = T.null

instance HasChars String where
  foldrChar     = foldr
  foldlChar     = foldl'
  splitLines    = lines . (++"\n")
  replicateChar = replicate
  isNull        = null

instance HasChars TL.Text where
  foldrChar         = TL.foldr
  foldlChar         = TL.foldl'
  splitLines        = TL.splitOn "\n"
  replicateChar n c = TL.replicate (fromIntegral n) (TL.singleton c)
  isNull            = TL.null

instance HasChars a => HasChars (Attributed a) where
  foldrChar _ acc Null = acc
  foldrChar f acc (Attr _ x) = foldrChar f acc x
  foldrChar f acc (Concattr x y) = foldrChar f (foldrChar f acc y) x
  foldlChar _ acc Null = acc
  foldlChar f acc (Attr _ x) = foldlChar f acc x
  foldlChar f acc (Concattr x y) = foldlChar f (foldlChar f acc x) y
  splitLines s              = reverse $ go ([], Null) [s]
    where
      split' Null           = Just []
      split' (Attr f x)     = Just (Attr f <$> splitLines x)
      split' Concattr{}     = Nothing
      go (lns, cur) []       = cur : lns
      go (lns, cur) [Null]   = cur : lns
      go (lns, cur) ((Concattr x y) : rest) = go (lns, cur) (x : y : rest)
      go (lns, cur) (b : bs) =
        case fromMaybe [] $ split' b of
          []      -> go (cur : lns, Null) bs
          [k1]    -> go (lns, cur <> k1) bs
          k1 : ks -> let (end, most) = fromMaybe (Null, []) $ uncons $ reverse ks in
                         go (most ++ (cur <> k1) : lns, end) bs
