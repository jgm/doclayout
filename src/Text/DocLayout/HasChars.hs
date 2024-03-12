{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.DocLayout.HasChars (HasChars(..)) where
import Prelude
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Text.DocLayout.Attributed
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

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
  build         :: a -> B.Builder
  build = foldrChar (mappend . B.singleton) (B.fromString "")

instance HasChars Text where
  foldrChar         = T.foldr
  foldlChar         = T.foldl'
  splitLines        = T.splitOn "\n"
  replicateChar n c = T.replicate n (T.singleton c)
  isNull            = T.null
  build             = B.fromText

instance HasChars String where
  foldrChar     = foldr
  foldlChar     = foldl'
  splitLines    = lines . (++"\n")
  replicateChar = replicate
  isNull        = null
  build         = B.fromString

instance HasChars TL.Text where
  foldrChar         = TL.foldr
  foldlChar         = TL.foldl'
  splitLines        = TL.splitOn "\n"
  replicateChar n c = TL.replicate (fromIntegral n) (TL.singleton c)
  isNull            = TL.null
  build             = B.fromLazyText

instance HasChars a => HasChars (Attr a) where
  foldrChar f a (Attr _ _ x) = foldrChar f a x
  foldlChar f a (Attr _ _ x) = foldlChar f a x
  splitLines (Attr l f x) = Attr l f <$> splitLines x
  build (Attr _ _ x) = build x

instance (HasChars a) => HasChars (Attributed a) where
  foldrChar _ acc (Attributed S.Empty) = acc
  foldrChar f acc (Attributed (xs :|> (Attr _ _ x))) =
    let l = foldrChar f acc x
        innerFold e a = foldrChar f a e
     in foldr innerFold l xs
  foldlChar _ acc (Attributed S.Empty) = acc
  foldlChar f acc (Attributed ((Attr _ _ x) :<| xs)) =
    let l = foldlChar f acc x
        innerFold e a = foldlChar f a e
     in foldr innerFold l xs
  splitLines (Attributed s) = fmap Attributed $ reverse $ go ([], S.empty) s
    where
      go (lns, cur) S.Empty = cur : lns
      go (lns, cur) (x :<| xs) =
        case splitLines x of
          []      -> go (cur : lns, S.empty) xs
          [k1]    -> go (lns, cur |> k1) xs
          k1 : ks ->
            let (end, most) = fromMaybe (S.empty, []) $ uncons $ reverse $ S.singleton <$> ks
             in go (most ++ (cur |> k1) : lns, end) xs
  build = foldMap build
