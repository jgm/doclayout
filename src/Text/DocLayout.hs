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
{- |
   Module      : Text.DocLayout
   Copyright   : Copyright (C) 2010-2019 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A prettyprinting library for the production of text documents,
including wrapped text, indentation and other prefixes, and
blocks for tables.
-}

module Text.DocLayout (
     -- * Rendering
       render
     , renderPlain
     , renderANSI
     -- * Doc constructors
     , cr
     , blankline
     , blanklines
     , space
     , literal
     , text
     , char
     , prefixed
     , flush
     , nest
     , hang
     , beforeNonBlank
     , nowrap
     , afterBreak
     , lblock
     , cblock
     , rblock
     , vfill
     , nestle
     , chomp
     , inside
     , braces
     , brackets
     , parens
     , quotes
     , doubleQuotes
     , bold
     , italic
     , underlined
     , strikeout
     , fg
     , bg
     , Color
     , black
     , red
     , green
     , yellow
     , blue
     , magenta
     , cyan
     , white
     , link
     , empty
     -- * Functions for concatenating documents
     , (<+>)
     , ($$)
     , ($+$)
     , hcat
     , hsep
     , vcat
     , vsep
     -- * Functions for querying documents
     , isEmpty
     , offset
     , minOffset
     , updateColumn
     , height
     , charWidth
     , realLength
     , realLengthNarrowContext
     , realLengthWideContext
     , realLengthNarrowContextNoShortcut
     , realLengthWideContextNoShortcut
     -- * Char properties
     , isSkinToneModifier
     , isEmojiVariation
     , isZWJ
     -- * Utility functions
     , unfoldD
     -- * Types
     , Doc(..)
     , HasChars(..)
     )

where
import Prelude
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Safe (lastMay, initSafe)
import Control.Monad
import Control.Monad.State.Strict
import GHC.Generics
import Data.Bifunctor (second)
import Data.Char (isSpace, ord)
import Data.List (foldl', intersperse)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as N
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Map.Internal as MInt
import Data.Data (Data, Typeable)
import Data.Foldable (toList)
import Data.String
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.DocLayout.HasChars
import Text.DocLayout.ANSIFont
import Text.DocLayout.Attributed
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup
#endif
import Text.Emoji (baseEmojis)


-- | Document, including structure relevant for layout.
data Doc a = Text Int a            -- ^ Text with specified width.
         | Block Int [Attributed a]           -- ^ A block with a width and lines.
         | VFill Int a             -- ^ A vertically expandable block;
                 -- when concatenated with a block, expands to height
                 -- of block, with each line containing the specified text.
         | CookedText Int (Attributed a) -- ^ Text which doesn't need further cooking
         | Prefixed Text (Doc a)   -- ^ Doc with each line prefixed with text.
                 -- Note that trailing blanks are omitted from the prefix
                 -- when the line after it is empty.
         | BeforeNonBlank (Doc a)  -- ^ Doc that renders only before nonblank.
         | Flush (Doc a)           -- ^ Doc laid out flush to left margin.
         | BreakingSpace           -- ^ A space or line break, in context.
         | AfterBreak Text         -- ^ Text printed only at start of line.
         | CarriageReturn          -- ^ Newline unless we're at start of line.
         | NewLine                 -- ^ newline.
         | BlankLines Int          -- ^ Ensure a number of blank lines.
         | Concat (Doc a) (Doc a)  -- ^ Two documents concatenated.
         | Styled StyleReq (Doc a)
         | Linked Text (Doc a)       -- ^ A hyperlink
         | Empty
         deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable,
                  Data, Typeable, Generic)


instance Semigroup (Doc a) where
  x <> Empty = x
  Empty <> x = x
  x <> y     = Concat x y

instance Monoid (Doc a) where
  mappend = (<>)
  mempty = Empty

instance HasChars a => IsString (Doc a) where
  fromString = text

-- | Unfold a 'Doc' into a flat list.
unfoldD :: Doc a -> [Doc a]
unfoldD Empty = []
unfoldD (Concat x@Concat{} y) = unfoldD x <> unfoldD y
unfoldD (Concat x y)          = x : unfoldD y
unfoldD x                     = [x]

-- | True if the document is empty.
isEmpty :: Doc a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | The empty document.
empty :: Doc a
empty = mempty

-- | Concatenate documents horizontally.
hcat :: [Doc a] -> Doc a
hcat = mconcat

-- | Concatenate a list of 'Doc's, putting breakable spaces
-- between them.
infixr 6 <+>
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> space <> y

-- | Same as 'hcat', but putting breakable spaces between the
-- 'Doc's.
hsep :: [Doc a] -> Doc a
hsep = foldr (<+>) empty

infixr 5 $$
-- | @a $$ b@ puts @a@ above @b@.
($$) :: Doc a -> Doc a -> Doc a
($$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> cr <> y

infixr 5 $+$
-- | @a $+$ b@ puts @a@ above @b@, with a blank line between.
($+$) :: Doc a -> Doc a -> Doc a
($+$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> blankline <> y

-- | List version of '$$'.
vcat :: [Doc a] -> Doc a
vcat = foldr ($$) empty

-- | List version of '$+$'.
vsep :: [Doc a] -> Doc a
vsep = foldr ($+$) empty

-- | Removes leading blank lines from a 'Doc'.
nestle :: Doc a -> Doc a
nestle d =
  case d of
    BlankLines _              -> Empty
    NewLine                   -> Empty
    Concat (Concat x y) z     -> nestle (Concat x (Concat y z))
    Concat BlankLines{} x     -> nestle x
    Concat NewLine x          -> nestle x
    _                         -> d

-- | Chomps trailing blank space off of a 'Doc'.
chomp :: Doc a -> Doc a
chomp d =
    case d of
    BlankLines _              -> Empty
    NewLine                   -> Empty
    CarriageReturn            -> Empty
    BreakingSpace             -> Empty
    Prefixed s d'             -> Prefixed s (chomp d')
    Concat (Concat x y) z     -> chomp (Concat x (Concat y z))
    Concat x y                ->
        case chomp y of
          Empty -> chomp x
          z     -> x <> z
    _                         -> d

-- Elements of a document with Styled and Linked subtrees flattened out into
-- a linear structure with open and close tags. An implementation detail of
-- the rendering process.
data FlatDoc a = FText Int a
               | FBlock Int [Attributed a]
               | FVFill Int a
               | FCookedText Int (Attributed a)
               | FPrefixed Text (NonEmpty (FlatDoc a))
               | FBeforeNonBlank (NonEmpty (FlatDoc a))
               | FFlush (NonEmpty (FlatDoc a))
               | FBreakingSpace
               | FAfterBreak (NonEmpty (FlatDoc a))
               | FCarriageReturn
               | FNewLine
               | FBlankLines Int
               | FStyleOpen StyleReq
               | FStyleClose
               | FLinkOpen Text
               | FLinkClose
         deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable,
                  Data, Typeable, Generic)

-- Given a Doc, return an equivalent list of FlatDocs, to be processed by
-- renderList. Worth noting:
--   * Treelike docs (Styled, and Linked) are turned into lists beginning
--     with an "open" tag and ending with a "close" tag, with the flattened
--     inner content in between.
--   * Other Docs with inner content are eliminated if the inner content is
--     empty, otherwise the inner content is itself flattened and made into
--     a NonEmpty.
flatten :: HasChars a => Doc a -> [FlatDoc a]
flatten (Text n a) = [FText n a]
flatten (Block n a) = [FBlock n a]
flatten (VFill n a) = [FVFill n a]
flatten (CookedText n a) = [FCookedText n a]
flatten (Prefixed p d) | null f = []
                       | otherwise = [FPrefixed p (N.fromList f)]
                       where f = (normalize . flatten) d
flatten (BeforeNonBlank d) | null f = []
                           | otherwise = [FBeforeNonBlank (N.fromList f)]
                           where f = flatten d
flatten (Flush d) | null f = []
                  | otherwise = [FFlush (N.fromList f)]
                  where f = flatten d
flatten BreakingSpace = [FBreakingSpace]
flatten CarriageReturn = [FCarriageReturn]
flatten (AfterBreak t) | null f = []
                       | otherwise = [FAfterBreak (N.fromList f)]
                       where f = flatten $ fromString $ T.unpack t
flatten NewLine = [FNewLine]
flatten (BlankLines n) = [FBlankLines n]
flatten Empty = []
flatten (Concat x y) = flatten x <> flatten y
flatten (Linked l x) = FLinkOpen l : flatten x <> [FLinkClose]
flatten (Styled f x) = FStyleOpen f : flatten x <> [FStyleClose]

type DocState a = State (RenderState a) ()

data RenderState a = RenderState{
         output     :: [Attr a]        -- ^ In reverse order
       , prefix     :: Text
       , usePrefix  :: Bool
       , lineLength :: Maybe Int  -- ^ 'Nothing' means no wrapping
       , column     :: Int
       , newlines   :: Int        -- ^ Number of preceding newlines
       , fontStack  :: [Font]
       , linkTarget :: Maybe Text -- ^ Current link target
       }

peekFont :: RenderState a -> Font
peekFont st = case fontStack st of
                [] -> baseFont
                x:_ -> x

newline :: HasChars a => DocState a
newline = do
  st' <- get
  let rawpref = prefix st'
  when (column st' == 0 && usePrefix st' && not (T.null rawpref)) $ do
     let pref = fromString $ T.unpack $ T.dropWhileEnd isSpace rawpref
     modify $ \st -> st{ output = Attr Nothing baseFont pref : output st
                       , column = column st + realLength pref }
  modify $ \st -> st { output = Attr Nothing baseFont "\n" : output st
                     , column = 0
                     , newlines = newlines st + 1
                     }

outp :: HasChars a => Int -> a -> DocState a
outp off s = do           -- offset >= 0 (0 might be combining char)
  st' <- get
  let pref = if usePrefix st' then fromString $ T.unpack $ prefix st' else mempty
  let font = peekFont st'
  when (column st' == 0 && not (isNull pref && font == baseFont)) $
    modify $ \st -> st{ output = Attr Nothing baseFont pref : output st
                    , column = column st + realLength pref }
  modify $ \st -> st{ output = Attr (linkTarget st) font s : output st
                    , column = column st + off
                    , newlines = 0 }

-- | Render a 'Doc'.  @render (Just n)@ will use
-- a line length of @n@ to reflow text on breakable spaces.
-- @render Nothing@ will not reflow text.
render :: HasChars a => Maybe Int -> Doc a -> a
render = renderPlain

renderANSI :: HasChars a => Maybe Int -> Doc a -> TL.Text
renderANSI n d = B.toLazyText $ go $ prerender n d where
  go s = (\(_,_,o) -> o) (go' s) <> B.fromText (renderFont baseFont) <> B.fromText (renderOSC8 Nothing)
  go' (Attributed s) = foldl attrRender (Nothing, baseFont, B.fromText "") s

renderPlain :: HasChars a => Maybe Int -> Doc a -> a
renderPlain n d = go $ prerender n d where
  go (Attributed s) = foldMap attrStrip s

attrStrip :: HasChars a => Attr a -> a
attrStrip (Attr _ _ y) | isNull y = ""
                       | otherwise = y

attrRender :: HasChars a => (Link, Font, B.Builder) -> Attr a -> (Link, Font, B.Builder)
attrRender (l, f, acc) (Attr m g y)
    | isNull y = (l, f, acc)
    | otherwise = (m, g, acc <> B.fromText newFont <> B.fromText newLink <> build y)
  where
    newLink = if l == m then mempty else renderOSC8 m
    newFont = if f == g then mempty else renderFont g

prerender :: HasChars a => Maybe Int -> Doc a -> Attributed a
prerender linelen doc = fromList . reverse . output $
  execState (renderDoc doc) startingState
   where startingState = RenderState{
                            output = mempty
                          , prefix = mempty
                          , usePrefix = True
                          , lineLength = linelen
                          , column = 0
                          , newlines = 2
                          , fontStack = []
                          , linkTarget = Nothing }

renderDoc :: HasChars a => Doc a -> DocState a
renderDoc = renderList . normalize . flatten


normalize :: HasChars a => [FlatDoc a] -> [FlatDoc a]
normalize [] = []
normalize [FNewLine] = normalize [FCarriageReturn]
normalize [FBlankLines _] = normalize [FCarriageReturn]
normalize [FBreakingSpace] = []
normalize (FBlankLines m : FBlankLines n : xs) =
  normalize (FBlankLines (max m n) : xs)
normalize (FBlankLines num : FBreakingSpace : xs) =
  normalize (FBlankLines num : xs)
normalize (FBlankLines m : FCarriageReturn : xs) = normalize (FBlankLines m : xs)
normalize (FBlankLines m : FNewLine : xs) = normalize (FBlankLines m : xs)
normalize (FNewLine : FBlankLines m : xs) = normalize (FBlankLines m : xs)
normalize (FNewLine : FBreakingSpace : xs) = normalize (FNewLine : xs)
normalize (FNewLine : FCarriageReturn : xs) = normalize (FNewLine : xs)
normalize (FCarriageReturn : FCarriageReturn : xs) =
  normalize (FCarriageReturn : xs)
normalize (FCarriageReturn : FBlankLines m : xs) = normalize (FBlankLines m : xs)
normalize (FCarriageReturn : FBreakingSpace : xs) =
  normalize (FCarriageReturn : xs)
normalize (FBreakingSpace : FCarriageReturn : xs) =
  normalize (FCarriageReturn:xs)
normalize (FBreakingSpace : FNewLine : xs) = normalize (FNewLine:xs)
normalize (FBreakingSpace : FBlankLines n : xs) = normalize (FBlankLines n:xs)
normalize (FBreakingSpace : FBreakingSpace : xs) = normalize (FBreakingSpace:xs)
normalize (x:xs) = x : normalize xs

mergeBlocks :: HasChars a => Int -> (Int, [a]) -> (Int, [a]) -> (Int, [a])
mergeBlocks h (w1,lns1) (w2,lns2) =
  (w, zipWith (\l1 l2 -> pad w1 l1 <> l2) lns1' lns2')
 where
  w  = w1 + w2
  len1 = length $ take h lns1  -- note lns1 might be infinite
  len2 = length $ take h lns2
  lns1' = if len1 < h
             then lns1 ++ replicate (h - len1) mempty
             else take h lns1
  lns2' = if len2 < h
             then lns2 ++ replicate (h - len2) mempty
             else take h lns2
  pad n s = s <> replicateChar (n - realLength s) ' '

renderList :: HasChars a => [FlatDoc a] -> DocState a
renderList [] = return ()

renderList (FText off s : xs) = do
  outp off s
  renderList xs

renderList (FCookedText off s : xs) = do
  st' <- get
  let pref = if usePrefix st' then fromString $ T.unpack $ prefix st' else mempty
  let elems (Attributed x) = reverse $ toList x
  when (column st' == 0 && not (isNull pref))  $
    modify $ \st -> st{ output = Attr Nothing baseFont pref : output st
                      , column = column st + realLength pref }
  modify $ \st -> st{ output = elems s ++ output st
                    , column = column st + off
                    , newlines = 0 }
  renderList xs

-- FStyleOpen and FStyleClose are balanced by construction when we create
-- them in `flatten`, so we can just pop the stack when we encounter
-- FStyleClose
renderList (FStyleOpen style : xs) = do
  st <- get
  let prevFont = peekFont st
  let nextFont = prevFont ~> style
  modify $ \s -> s{fontStack = nextFont : fontStack s}
  renderList xs

renderList (FStyleClose : xs) = do
  modify $ \s -> s{fontStack = drop 1 $ fontStack s}
  renderList xs

-- Nested links are nonsensical, we only handle the outermost and
-- silently ignore any attempts to have a link inside a link

-- Nested links are nonsensical, we only handle the outermost and
-- silently ignore any attempts to have a link inside a link
renderList (FLinkOpen target : xs) = do
  st <- get
  case linkTarget st of
    Nothing -> do
      modify $ \s -> s{linkTarget = Just target}
      renderList xs
    _ -> do
      let (next, rest) = break isLinkClose xs
      renderList (next <> drop 1 rest)
  where
    isLinkClose FLinkClose = True
    isLinkClose _ = False

renderList (FLinkClose : xs) = do
  modify $ \s -> s{linkTarget = Nothing}
  renderList xs

renderList (FPrefixed pref d : xs) = do
  st <- get
  let oldPref = prefix st
  put st{ prefix = prefix st <> pref }
  renderList $ normalize $ N.toList d
  modify $ \s -> s{ prefix = oldPref }
  -- renderDoc CarriageReturn
  renderList xs

renderList (FFlush d : xs) = do
  st <- get
  let oldUsePrefix = usePrefix st
  put st{ usePrefix = False }
  renderList $ normalize $ N.toList d
  modify $ \s -> s{ usePrefix = oldUsePrefix }
  renderList xs

renderList (FBeforeNonBlank d : xs) = do
  let next = dropWhile (not . isPrintable) xs
  case next of
    (x:_) | startsBlank x -> renderList xs
          | otherwise     -> renderList (normalize $ N.toList d) >> renderList xs
    []                    -> renderList xs

renderList (FBlankLines num : xs) = do
  st <- get
  case output st of
     _ | newlines st > num -> return ()
       | otherwise -> replicateM_ (1 + num - newlines st) newline
  renderList xs

renderList (FCarriageReturn : xs) = do
  st <- get
  if newlines st > 0
     then renderList xs
     else do
       newline
       renderList xs

renderList (FNewLine : xs) = do
  newline
  renderList xs

renderList (FBreakingSpace : xs) = do
  let isBreakingSpace FBreakingSpace = True
      isBreakingSpace _ = False
  let xs' = dropWhile isBreakingSpace xs
  let next = takeWhile (not . isBreakable) xs'
  st <- get
  let off = foldl' (\tot t -> tot + offsetOf t) 0 next
  case lineLength st of
        Just l | column st + 1 + off > l -> newline
        _  -> when (column st > 0) $ outp 1 " "
  renderList xs'

renderList (FAfterBreak t : xs) = do
  st <- get
  if newlines st > 0
     then renderList (toList t <> xs)
     else renderList xs

-- FBlock and FVFill are all that's left
renderList (b : xs) = do
  st <- get
  let font = peekFont st
  let (bs, rest) = span isBlock xs
  -- ensure we have right padding unless end of line
  let heightOf (FBlock _ ls) = length ls
      heightOf _            = 1
  let maxheight = maximum $ map heightOf (b:bs)
  let toBlockSpec (FBlock w ls) = (w, ls)
      toBlockSpec (FVFill w t)  = (w, map (singleton . (Attr (linkTarget st) font)) (take maxheight $ repeat t))
      toBlockSpec _            = (0, [])
  let (_, lns') = foldl (mergeBlocks maxheight) (toBlockSpec b)
                             (map toBlockSpec bs)
  let oldPref = prefix st
  case column st - realLength oldPref of
        n | n > 0 -> modify $ \s -> s{ prefix = oldPref <> T.replicate n " " }
        _ -> return ()
  renderList $ intersperse FCarriageReturn (mapMaybe cook lns')
  modify $ \s -> s{ prefix = oldPref }
  renderList rest

isBreakable :: HasChars a => FlatDoc a -> Bool
isBreakable FBreakingSpace      = True
isBreakable FCarriageReturn     = True
isBreakable FNewLine            = True
isBreakable (FBlankLines _)     = True
isBreakable _                  = False

startsBlank' :: HasChars a => a -> Bool
startsBlank' t = fromMaybe False $ foldlChar go Nothing t
  where
   go Nothing  c = Just (isSpace c)
   go (Just b) _ = Just b

startsBlank :: HasChars a => FlatDoc a -> Bool
startsBlank (FText _ t)                = startsBlank' t
startsBlank (FCookedText _ t)          = startsBlank' t
startsBlank (FBlock n ls)              = n > 0 && all startsBlank' ls
startsBlank (FVFill n t)               = n > 0 && startsBlank' t
startsBlank (FBeforeNonBlank (x :| _)) = startsBlank x
startsBlank (FPrefixed _ (x :| _))     = startsBlank x
startsBlank (FFlush (x :| _))          = startsBlank x
startsBlank FBreakingSpace             = True
startsBlank (FAfterBreak (t :| _))     = startsBlank t
startsBlank FCarriageReturn            = True
startsBlank FNewLine                   = True
startsBlank (FBlankLines _)            = True
startsBlank (FStyleOpen _)             = True
startsBlank (FLinkOpen _)              = True
startsBlank FStyleClose                = True
startsBlank FLinkClose                 = True

isPrintable :: FlatDoc a -> Bool
isPrintable FLinkOpen{} = False
isPrintable FLinkClose{} = False
isPrintable FStyleOpen{} = False
isPrintable FStyleClose{} = False
isPrintable _ = True

isBlock :: FlatDoc a -> Bool
isBlock FBlock{} = True
isBlock FVFill{} = True
isBlock _       = False

offsetOf :: FlatDoc a -> Int
offsetOf (FText o _)       = o
offsetOf (FBlock w _)      = w
offsetOf (FVFill w _)      = w
offsetOf (FCookedText w _) = w
offsetOf FBreakingSpace    = 1
offsetOf _                = 0

-- | Create a 'Doc' from a stringlike value.
literal :: HasChars a => a -> Doc a
literal x =
  mconcat $
    intersperse NewLine $
      map (\s -> if isNull s
                    then Empty
                    else let !len = realLength s
                          in Text len s) $
        splitLines x
{-# NOINLINE literal #-}

cook :: HasChars a => Attributed a -> Maybe (FlatDoc a)
cook x | isNull x = Nothing
       | otherwise = let !len = realLength x in Just (FCookedText len x)

-- | A literal string.  (Like 'literal', but restricted to String.)
text :: HasChars a => String -> Doc a
text = literal . fromString

-- | A character.
char :: HasChars a => Char -> Doc a
char c = text $ fromString [c]

-- | A breaking (reflowable) space.
space :: Doc a
space = BreakingSpace

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc a
cr = CarriageReturn

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
blankline :: Doc a
blankline = BlankLines 1

-- | Inserts blank lines unless they exist already.
-- (@blanklines m <> blanklines n@ has the same effect as @blanklines (max m n)@.
blanklines :: Int -> Doc a
blanklines = BlankLines

-- | Uses the specified string as a prefix for every line of
-- the inside document (except the first, if not at the beginning
-- of the line).
prefixed :: IsString a => String -> Doc a -> Doc a
prefixed pref doc
  | isEmpty doc = Empty
  | otherwise   = Prefixed (fromString pref) doc

-- | Makes a 'Doc' flush against the left margin.
flush :: Doc a -> Doc a
flush doc
  | isEmpty doc = Empty
  | otherwise   = Flush doc

-- | Indents a 'Doc' by the specified number of spaces.
nest :: IsString a => Int -> Doc a -> Doc a
nest ind = prefixed (replicate ind ' ')

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: IsString a => Int -> Doc a -> Doc a -> Doc a
hang ind start doc = start <> nest ind doc

-- | @beforeNonBlank d@ conditionally includes @d@ unless it is
-- followed by blank space.
beforeNonBlank :: Doc a -> Doc a
beforeNonBlank = BeforeNonBlank

-- | Makes a 'Doc' non-reflowable.
nowrap :: IsString a => Doc a -> Doc a
nowrap = mconcat . map replaceSpace . unfoldD
  where replaceSpace BreakingSpace = Text 1 $ fromString " "
        replaceSpace x             = x

-- | Content to print only if it comes at the beginning of a line,
-- to be used e.g. for escaping line-initial `.` in roff man.
afterBreak :: Text -> Doc a
afterBreak = AfterBreak

-- | Returns the width of a 'Doc'.
offset :: (IsString a, HasChars a) => Doc a -> Int
offset = uncurry max . getOffset (const False) (0, 0)

-- | Returns the minimal width of a 'Doc' when reflowed at breakable spaces.
minOffset :: HasChars a => Doc a -> Int
minOffset = uncurry max . getOffset (> 0) (0,0)

-- l = longest, c = current
getOffset :: (IsString a, HasChars a)
          => (Int -> Bool) -> (Int, Int) -> Doc a -> (Int, Int)
getOffset breakWhen (!l, !c) x =
  case x of
    Text n _ -> (l, c + n)
    Block n _ -> (l, c + n)
    VFill n _ -> (l, c + n)
    CookedText n _ -> (l, c + n)
    Empty -> (l, c)
    Styled _ d -> getOffset breakWhen (l, c) d
    Linked _ d -> getOffset breakWhen (l, c) d
    CarriageReturn -> (max l c, 0)
    NewLine -> (max l c, 0)
    BlankLines _ -> (max l c, 0)
    Prefixed t d ->
      let (l',c') = getOffset breakWhen (0, 0) d
       in (max l (l' + realLength t), c' + realLength t)
    BeforeNonBlank _ -> (l, c)
    Flush d -> getOffset breakWhen (l, c) d
    BreakingSpace
      | breakWhen c -> (max l c, 0)
      | otherwise -> (l, c + 1)
    AfterBreak t -> if c == 0
                       then (l, c + realLength t)
                       else (l, c)
    Concat (Concat d y) z ->
      getOffset breakWhen (l, c) (Concat d (Concat y z))
    Concat (BeforeNonBlank d) y ->
      if isNonBlank y
         then getOffset breakWhen (l, c) (Concat d y)
         else getOffset breakWhen (l, c) y
    Concat d y ->
      let (l', c') = getOffset breakWhen (l, c) d
       in getOffset breakWhen (l', c') y

isNonBlank :: Doc a -> Bool
isNonBlank (Text _ _) = True
isNonBlank (BeforeNonBlank d) = isNonBlank d
isNonBlank (Flush d) = isNonBlank d
isNonBlank (Concat d _) = isNonBlank d
isNonBlank _ = False

-- | Returns the column that would be occupied by the last
-- laid out character (assuming no wrapping).
updateColumn :: HasChars a => Doc a -> Int -> Int
updateColumn d k = snd . getOffset (const False) (0,k) $ d

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left.
lblock :: HasChars a => Int -> Doc a -> Doc a
lblock = block id

-- | Like 'lblock' but aligned to the right.
rblock :: HasChars a => Int -> Doc a -> Doc a
rblock w = block (\s -> replicateChar (w - realLength s) ' ' <> s) w

-- | Like 'lblock' but aligned centered.
cblock :: HasChars a => Int -> Doc a -> Doc a
cblock w = block (\s -> replicateChar ((w - realLength s) `div` 2) ' ' <> s) w

-- | Returns the height of a block or other 'Doc'.
height :: HasChars a => Doc a -> Int
height = length . splitLines . render Nothing

block :: HasChars a => (Attributed a -> Attributed a) -> Int -> Doc a -> Doc a
block filler width d
  | width < 1 && not (isEmpty d) = block filler 1 d
  | otherwise                    = Block width ls
     where
       preimage = prerender (Just width) d
       reboxed = chop width preimage
       ls = map filler reboxed

-- | An expandable border that, when placed next to a box,
-- expands to the height of the box.  Strings cycle through the
-- list provided.
vfill :: HasChars a => a -> Doc a
vfill t = VFill (realLength t) t

chop :: HasChars a => Int -> a -> [a]
chop n =
   concatMap chopLine . removeFinalEmpty . map addRealLength . splitLines
 where
   removeFinalEmpty xs = case lastMay xs of
                           Just (0, _) -> initSafe xs
                           _           -> xs
   addRealLength l = (realLength l, l)
   chopLine (len, l)
     | len <= n  = [l]
     | otherwise = map snd $
                    foldrChar
                     (\c ls ->
                       let clen = charWidth c
                           cs = replicateChar 1 c
                        in case ls of
                             (len', l'):rest
                               | len' + clen > n ->
                                   (clen, cs):(len', l'):rest
                               | otherwise ->
                                   (len' + clen, cs <> l'):rest
                             [] -> [(clen, cs)]) [] l

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: Doc a -> Doc a -> Doc a -> Doc a
inside start end contents =
  start <> contents <> end

-- | Puts a 'Doc' in curly braces.
braces :: HasChars a => Doc a -> Doc a
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: HasChars a => Doc a -> Doc a
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: HasChars a => Doc a -> Doc a
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: HasChars a => Doc a -> Doc a
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: HasChars a => Doc a -> Doc a
doubleQuotes = inside (char '"') (char '"')

styled :: HasChars a => StyleReq -> Doc a -> Doc a
styled _ Empty = Empty
styled s x = Styled s x

bold :: HasChars a => Doc a -> Doc a
bold = styled (RWeight Bold)

italic :: HasChars a => Doc a -> Doc a
italic = styled (RShape Italic)

underlined :: HasChars a => Doc a -> Doc a
underlined = styled (RUnderline ULSingle)

strikeout :: HasChars a => Doc a -> Doc a
strikeout = styled (RStrikeout Struck)

-- The Color type is here as an opaque alias to Color8 for the public interface
-- and there's trivial smart constructors for the individual colors to
-- hopefully allow for easier extension to supporting indexed and rgb colors in
-- the future, without dramatically changing the public API.

type Color = Color8

fg :: HasChars a => Color -> Doc a -> Doc a
fg = styled . RForeground . FG

bg :: HasChars a => Color -> Doc a -> Doc a
bg = styled . RBackground . BG

blue :: Color
blue = Blue

black :: Color
black = Black

red :: Color
red = Red

green :: Color
green = Green

yellow :: Color
yellow = Yellow

magenta :: Color
magenta = Magenta

cyan :: Color
cyan = Cyan

white :: Color
white = White

link :: HasChars a => Text -> Doc a -> Doc a
link = Linked

-- | Returns width of a character in a monospace font:  0 for a combining
-- character, 1 for a regular character, 2 for an East Asian wide character.
-- Ambiguous characters are treated as width 1.
charWidth :: Char -> Int
charWidth = extractLength . updateMatchStateNarrow (MatchState False 0 ' ' 0)

-- | Get real length of string, taking into account combining and double-wide
-- characters. Ambiguous characters are treated as width 1.
realLength :: HasChars a => a -> Int
realLength = realLengthNarrowContext

-- | Get the real length of a string, taking into account combining and
-- double-wide characters. Ambiguous characters are treated as width 1.
realLengthNarrowContext :: HasChars a => a -> Int
realLengthNarrowContext = realLengthWith updateMatchStateNarrow

-- | Get the real length of a string, taking into account combining and
-- double-wide characters. Ambiguous characters are treated as width 2.
realLengthWideContext :: HasChars a => a -> Int
realLengthWideContext = realLengthWith updateMatchStateWide

-- | Like 'realLengthNarrowContext', but avoids optimizations (shortcuts).
-- This is exposed for testing, to ensure that the optimizations are safe.
realLengthNarrowContextNoShortcut :: HasChars a => a -> Int
realLengthNarrowContextNoShortcut = realLengthWith updateMatchStateNoShortcut

-- | Like 'realLengthWideContext', but avoids optimizations (shortcuts).
-- This is exposed for testing, to ensure that the optimizations are safe.
realLengthWideContextNoShortcut :: HasChars a => a -> Int
realLengthWideContextNoShortcut = realLengthWith updateMatchStateNoShortcutWide

-- | Get real length of string, taking into account combining and double-wide
-- characters, using the given accumulator. This is exposed for testing.
realLengthWith :: HasChars a => (MatchState -> Char -> MatchState) -> a -> Int
realLengthWith f = extractLength . foldlChar f (MatchState True 0 ' ' 0)

-- | Update a 'MatchState' by processing a character.
-- For efficiency, we isolate commonly used portions of the basic
-- multilingual plane that do not have emoji in them.
-- This works in a narrow context.
updateMatchStateNarrow :: MatchState -> Char -> MatchState
updateMatchStateNarrow (MatchState firstChar tot _ tentative) !c
    -- Control characters have width 0: friends don't let friends use tabs
    | c <= '\x001F'  = controlState
    -- ASCII
    | c <= '\x007E'  = narrowState
    -- More control characters
    | c <= '\x009F'  = controlState
    -- Extended Latin: Latin 1-supplement, Extended-A, Extended-B, IPA Extensions.
    -- This block is full of ambiguous characters, so these shortcuts will not
    -- work in a wide context.
    | c == '\x00AD'  = controlState    -- Soft hyphen
    | c <= '\x02FF'  = narrowState
    -- Combining diacritical marks used in Latin and other scripts
    | c <= '\x036F'  = combiningState
    -- Han ideographs
    | c >= '\x3250' && c <= '\xA4CF' =
        if | c <= '\x4DBF' -> wideState       -- Han ideographs
           | c <= '\x4DFF' -> narrowState     -- Hexagrams
           | otherwise     -> wideState       -- More Han ideographs
    -- Arabic
    | c >= '\x0600' && c <= '\x06FF' =
        if | c <= '\x0605' -> controlState    -- Number marks
           | c <= '\x060F' -> narrowState     -- Punctuation and marks
           | c <= '\x061A' -> combiningState  -- Combining marks
           | c == '\x061B' -> narrowState     -- Arabic semicolon
           | c <= '\x061C' -> controlState    -- Letter mark
           | c <= '\x064A' -> narrowState     -- Main Arabic abjad
           | c <= '\x065F' -> combiningState  -- Arabic vowel markers
           | c == '\x0670' -> combiningState  -- Superscript alef
           | c <= '\x06D5' -> narrowState     -- Arabic digits and letters used in other languages
           | c <= '\x06DC' -> combiningState  -- Small high ligatures
           | c == '\x06DD' -> controlState    -- End of ayah
           | c == '\x06DE' -> narrowState     -- Start of rub el hizb
           | c <= '\x06E4' -> combiningState  -- More small high ligatures
           | c <= '\x06E6' -> narrowState     -- Small vowels
           | c == '\x06E9' -> narrowState     -- Place of sajdah
           | c <= '\x06ED' -> combiningState  -- More combining
           | otherwise     -> narrowState     -- All the rest
    -- Devanagari
    | c >= '\x0900' && c <= '\x097F' =
        if | c <= '\x0902' -> combiningState  -- Combining characters
           | c <= '\x0939' -> narrowState     -- Main Devanagari abugida
           | c == '\x093A' -> combiningState
           | c == '\x093C' -> combiningState
           | c <= '\x0940' -> narrowState     -- Main Devanagari abugida
           | c <= '\x0948' -> combiningState  -- Combining characters
           | c == '\x094D' -> combiningState  -- Combining characters
           | c <= '\x0950' -> narrowState     -- Devanagari om
           | c <= '\x0957' -> combiningState  -- Combining characters
           | c == '\x0962' -> combiningState  -- Combining character
           | c == '\x0963' -> combiningState  -- Combining character
           | otherwise     -> narrowState     -- Devanagari digits and up to beginning of Bengali block
    -- Bengali (plus a couple Gurmukhi characters)
    | c >= '\x0980' && c <= '\x0A02' =
        if | c == '\x0981' -> combiningState  -- Combining signs
           | c == '\x09BC' -> combiningState  -- Combining signs
           | c <= '\x09C0' -> narrowState     -- Main Bengali abugida
           | c <= '\x09C4' -> combiningState  -- Combining signs
           | c == '\x09CD' -> combiningState  -- Combining signs
           | c <= '\x09E1' -> narrowState     -- Bengali
           | c <= '\x09E3' -> combiningState  -- Combining marks
           | c == '\x09E2' -> combiningState  -- Bengali vocalic vowel signs
           | c == '\x09E3' -> combiningState  -- Bengali vocalic vowel signs
           | c <= '\x09FD' -> narrowState     -- Bengali digits and other symbols
           | otherwise     -> combiningState  -- Bengali sandhi mark, plus a few symbols from Gurmukhi
    -- Cyrillic (plus Greek and Armenian for free)
    -- This block has many ambiguous characters, and so cannot be used in wide contexts
    | c >= '\x0370' && c <= '\x058F' =
        if | c <= '\x0482' -> narrowState     -- Main Greek and Cyrillic block
           | c <= '\x0489' -> combiningState  -- Cyrillic combining characters
           | otherwise     -> narrowState     -- Extra Cyrillic characters used in Ukrainian and others, plus Armenian
    -- Japanese
    | c >= '\x2E80' && c <= '\x324F' =
        if | c <= '\x3029' -> wideState       -- Punctuation and others
           | c <= '\x302D' -> combiningState  -- Tone marks
           | c == '\x303F' -> narrowState     -- Half-fill space
           | c <= '\x3096' -> wideState       -- Hiragana and others
           | c <= '\x309A' -> combiningState  -- Hiragana voiced marks
           | c <= '\x3247' -> wideState       -- Katakana plus compatibility Jamo for Korean
           | otherwise     -> ambiguousState  -- Circled numbers
    -- Korean
    | c >= '\xAC00' && c <= '\xD7A3' = wideState  -- Precomposed Hangul
    -- Telugu (plus one character of Kannada)
    | c >= '\x0C00' && c <= '\x0C80' =
        if | c == '\x0C00' -> combiningState  -- Combining characters
           | c == '\x0C04' -> combiningState  -- Combining characters
           | c <= '\x0C39' -> narrowState     -- Main Telugu abugida
           | c == '\x0C3D' -> narrowState     -- Telugu avagraha
           | c <= '\x0C40' -> combiningState  -- Vowel markers
           | c <= '\x0C44' -> narrowState     -- Vowel markers
           | c <= '\x0C56' -> combiningState  -- Vowel markers
           | c == '\x0C62' -> combiningState  -- Combining character
           | c == '\x0C63' -> combiningState  -- Combining character
           | otherwise     -> narrowState     -- Telugu digits
    -- Tamil
    | c >= '\x0B80' && c <= '\x0BFF' =
        if | c <= '\x0B82' -> combiningState  -- Combining characters
           | c == '\x0BC0' -> combiningState  -- Combining characters
           | c == '\x0BCD' -> combiningState  -- Vowel markers
           | c <= '\x0BCC' -> narrowState     -- Main Tamil abugida
           | otherwise     -> narrowState     -- Tamil digits and others
  where
    narrowState    = MatchState False (tot + tentative) c 1
    wideState      = MatchState False (tot + tentative) c 2
    combiningState = let w = if firstChar then 1 else 0 in MatchState False (tot + tentative) c w
    controlState   = MatchState False (tot + tentative) c 0
    ambiguousState = MatchState False (tot + tentative) c 1
updateMatchStateNarrow s c = updateMatchStateNoShortcut s c

-- | Update a 'MatchState' by processing a character.
-- For efficiency, we isolate commonly used portions of the basic
-- multilingual plane that do not have emoji in them.
-- This works in a wide context.
updateMatchStateWide :: MatchState -> Char -> MatchState
updateMatchStateWide (MatchState firstChar tot _ tentative) !c
    -- Control characters have width 0: friends don't let friends use tabs
    | c <= '\x001F'  = controlState
    -- ASCII
    | c <= '\x007E'  = narrowState
    -- Han ideographs
    | c >= '\x3250' && c <= '\xA4CF' =
        if | c <= '\x4DBF' -> wideState       -- Han ideographs
           | c <= '\x4DFF' -> narrowState     -- Hexagrams
           | otherwise     -> wideState       -- More Han ideographs
    -- Japanese
    | c >= '\x2E80' && c <= '\x324F' =
        if | c <= '\x3029' -> wideState       -- Punctuation and others
           | c <= '\x302D' -> combiningState  -- Tone marks
           | c == '\x303F' -> narrowState     -- Half-fill space
           | c <= '\x3096' -> wideState       -- Hiragana and others
           | c <= '\x309A' -> combiningState  -- Hiragana voiced marks
           | c <= '\x3247' -> wideState       -- Katakana plus compatibility Jamo for Korean
           | otherwise     -> ambiguousState  -- Circled numbers
    -- Korean
    | c >= '\xAC00' && c <= '\xD7A3' = wideState  -- Precomposed Hangul
    -- Combining diacritical marks used in Latin and other scripts
    | c >= '\x0300' && c <= '\x036F'  = combiningState
    -- Arabic
    | c >= '\x0600' && c <= '\x06FF' =
        if | c <= '\x0605' -> controlState    -- Number marks
           | c <= '\x060F' -> narrowState     -- Punctuation and marks
           | c <= '\x061A' -> combiningState  -- Combining marks
           | c == '\x061B' -> narrowState     -- Arabic semicolon
           | c <= '\x061C' -> controlState    -- Letter mark
           | c <= '\x064A' -> narrowState     -- Main Arabic abjad
           | c <= '\x065F' -> combiningState  -- Arabic vowel markers
           | c == '\x0670' -> combiningState  -- Superscript alef
           | c <= '\x06D5' -> narrowState     -- Arabic digits and letters used in other languages
           | c <= '\x06DC' -> combiningState  -- Small high ligatures
           | c == '\x06DD' -> controlState    -- End of ayah
           | c == '\x06DE' -> narrowState     -- Start of rub el hizb
           | c <= '\x06E4' -> combiningState  -- More small high ligatures
           | c <= '\x06E6' -> narrowState     -- Small vowels
           | c == '\x06E9' -> narrowState     -- Place of sajdah
           | c <= '\x06ED' -> combiningState  -- More combining
           | otherwise     -> narrowState     -- All the rest
    -- Devanagari
    | c >= '\x0900' && c <= '\x097F' =
        if | c <= '\x0902' -> combiningState  -- Combining characters
           | c <= '\x0939' -> narrowState     -- Main Devanagari abugida
           | c == '\x093A' -> combiningState
           | c == '\x093C' -> combiningState
           | c <= '\x0940' -> narrowState     -- Main Devanagari abugida
           | c <= '\x0948' -> combiningState  -- Combining characters
           | c == '\x094D' -> combiningState  -- Combining characters
           | c <= '\x0950' -> narrowState     -- Devanagari om
           | c <= '\x0957' -> combiningState  -- Combining characters
           | c == '\x0962' -> combiningState  -- Combining character
           | c == '\x0963' -> combiningState  -- Combining character
           | otherwise     -> narrowState     -- Devanagari digits and up to beginning of Bengali block
    -- Bengali (plus a couple Gurmukhi characters)
    | c >= '\x0980' && c <= '\x0A02' =
        if | c == '\x0981' -> combiningState  -- Combining signs
           | c == '\x09BC' -> combiningState  -- Combining signs
           | c <= '\x09C0' -> narrowState     -- Main Bengali abugida
           | c <= '\x09C4' -> combiningState  -- Combining signs
           | c == '\x09CD' -> combiningState  -- Combining signs
           | c <= '\x09E1' -> narrowState     -- Bengali
           | c <= '\x09E3' -> combiningState  -- Combining marks
           | c == '\x09E2' -> combiningState  -- Bengali vocalic vowel signs
           | c == '\x09E3' -> combiningState  -- Bengali vocalic vowel signs
           | c <= '\x09FD' -> narrowState     -- Bengali digits and other symbols
           | otherwise     -> combiningState  -- Bengali sandhi mark, plus a few symbols from Gurmukhi
    -- Telugu (plus one character of Kannada)
    | c >= '\x0C00' && c <= '\x0C80' =
        if | c == '\x0C00' -> combiningState  -- Combining characters
           | c == '\x0C04' -> combiningState  -- Combining characters
           | c <= '\x0C39' -> narrowState     -- Main Telugu abugida
           | c == '\x0C3D' -> narrowState     -- Telugu avagraha
           | c <= '\x0C40' -> combiningState  -- Vowel markers
           | c <= '\x0C44' -> narrowState     -- Vowel markers
           | c <= '\x0C56' -> combiningState  -- Vowel markers
           | c == '\x0C62' -> combiningState  -- Combining character
           | c == '\x0C63' -> combiningState  -- Combining character
           | otherwise     -> narrowState     -- Telugu digits
    -- Tamil
    | c >= '\x0B80' && c <= '\x0BFF' =
        if | c <= '\x0B82' -> combiningState  -- Combining characters
           | c == '\x0BC0' -> combiningState  -- Combining characters
           | c == '\x0BCD' -> combiningState  -- Vowel markers
           | c <= '\x0BCC' -> narrowState     -- Main Tamil abugida
           | otherwise     -> narrowState     -- Tamil digits and others
  where
    narrowState    = MatchState False (tot + tentative) c 1
    wideState      = MatchState False (tot + tentative) c 2
    combiningState = let w = if firstChar then 1 else 0 in MatchState False (tot + tentative) c w
    controlState   = MatchState False (tot + tentative) c 0
    ambiguousState = MatchState False (tot + tentative) c 2
updateMatchStateWide s c = updateMatchStateNoShortcutWide s c

-- | Update a 'MatchState' by processing a character, without taking any
-- shortcuts. This should give the same answer as 'updateMatchStateNarrow', but will
-- be slower. It is here to test that the shortcuts are implemented correctly.
updateMatchStateNoShortcut :: MatchState -> Char -> MatchState
updateMatchStateNoShortcut match c = resolveWidth match c $ unicodeWidth (unicodeRangeMap Narrow) c

-- | Update a 'MatchState' by processing a character, without taking any
-- shortcuts. This should give the same answer as 'updateMatchStateWide', but will
-- be slower. It is here to test that the shortcuts are implemented correctly.
updateMatchStateNoShortcutWide :: MatchState -> Char -> MatchState
updateMatchStateNoShortcutWide match c = resolveWidth match c $ unicodeWidth (unicodeRangeMap Wide) c

-- | Update a match state given a character and its class
resolveWidth :: MatchState -> Char -> UnicodeWidth -> MatchState
resolveWidth (MatchState firstChar tot lastChar tentative) !c = \case
    Narrow                    -> narrowState
    Wide                      -> wideState
    Combining                 -> combiningState
    Control                   -> controlState
    Ambiguous                 -> ambiguousState
    -- Zero width joiners will join two emoji together, so let's discard the
    -- state and parse the next emoji
    ZWJ | isLastCharEmojiLike -> MatchState False (tot - 2) c 2
    ZWJ                       -> controlState
    -- Variation modifiers modify the emoji up to this point, so can be
    -- discarded. However, they always make it width 2, so we set the tentative
    -- width to 2.
    EmojiPresentationMod | Just (EmojiInfo True _) <- lastCharEmoji
                              -> MatchState False tot c 2
    EmojiPresentationMod      -> controlState
    -- Skin tone modifiers make it width 2, but if they are not in a valid
    -- position they end the emoji and take up another width 2.
    EmojiSkinToneMod | Just (EmojiInfo _ True) <- lastCharEmoji
                              -> MatchState False tot c 2
    EmojiSkinToneMod          -> wideState
  where
    narrowState    = MatchState False (tot + tentative) c 1
    wideState      = MatchState False (tot + tentative) c 2
    combiningState = let w = if firstChar then 1 else 0 in MatchState False (tot + tentative) c w
    controlState   = MatchState False (tot + tentative) c 0
    ambiguousState = MatchState False (tot + tentative) c 1  -- Should be handled already, but treat it as 1
    lastCharEmoji = IM.lookup (ord lastChar) emojiMap
    isLastCharEmojiLike = isJust lastCharEmoji || lastChar == '\xFE0F' || isSkinToneModifier lastChar

-- | Keeps track of state in length calculations, determining whether we're at
-- the first character, the width so far, possibly a tentative width for this
-- group, and the Map for possible emoji continuations.
data MatchState = MatchState
    { matchIsFirst       :: !Bool
    , matchTotal         :: !Int
    , matchLastChar      :: !Char
    , matchTentative     :: !Int
    }
  deriving (Show)

-- | Get the final width from a 'MatchState'.
extractLength :: MatchState -> Int
extractLength (MatchState _ tot _ tentative) = tot + tentative

-- | The unicode width  of a given character.
data UnicodeWidth = Narrow | Wide | Combining | Control | Ambiguous
                  | ZWJ | EmojiPresentationMod | EmojiSkinToneMod
  deriving (Show, Eq)

-- | Checks whether a character is a skin tone modifier.
isSkinToneModifier :: Char -> Bool
isSkinToneModifier c = c >= '\x1F3FB' && c <= '\x1F3FF'

-- | Checks whether a character is an emoji variation modifier.
isEmojiVariation :: Char -> Bool
isEmojiVariation c = c >= '\xFE0E' && c <= '\xFE0F'

-- | Checks whether a character is a zero-width joiner.
isZWJ :: Char -> Bool
isZWJ c = c == '\x200D'

data EmojiInfo = EmojiInfo
    { acceptsVariation :: !Bool
    , acceptsSkinTones :: !Bool
    } deriving (Eq, Show)

instance Semigroup EmojiInfo where
    EmojiInfo v1 s1 <> EmojiInfo v2 s2 = EmojiInfo (v1 || v2) (s1 || s2)

-- | Check a character to see how it modifies emoji.
variationState :: Char -> EmojiInfo
variationState y = EmojiInfo (isEmojiVariation y) (isSkinToneModifier y)

-- | A map of all emoji start characters and the modifiers they take.
emojiMap :: IM.IntMap EmojiInfo
emojiMap = foldl' (flip addEmoji) mempty $ mapMaybe T.uncons baseEmojis
  where
    addEmoji (x, xs) = IM.insertWith (<>) (ord x) (emojiInfo xs)
    emojiInfo = maybe (EmojiInfo False False) (variationState . fst) . T.uncons

-- | Denotes the contiguous ranges of Unicode characters which have a given
-- width: 1 for a regular character, 2 for an East Asian wide character.
-- Ambiguous characters are resolved in the specified way.
unicodeRangeMap :: UnicodeWidth -> UnicodeMap
unicodeRangeMap ambiguous =
    repack . addEmojiClasses . M.fromList . mergeRanges $
    map (second resolve) unicodeSpec
  where
    resolve Ambiguous = ambiguous
    resolve x         = x

-- | Add zero-width joiner and emoji modifiers to a Map.
addEmojiClasses :: M.Map Char UnicodeWidth -> M.Map Char UnicodeWidth
addEmojiClasses =
    addAndRestoreBoundary '\x200D' '\x200D' ZWJ
    . addAndRestoreBoundary '\xFE0F' '\xFE0F' EmojiPresentationMod
    . addAndRestoreBoundary '\x1F3FB' '\x1F3FF' EmojiSkinToneMod
  where
    addAndRestoreBoundary k1 k2 v m = insertAfter $ M.insert k1 v m
      where
        insertAfter = case M.lookupLE k1 m of
          Just (_, prev) -> M.insertWith (\_ old -> old) (succ k2) prev
          Nothing        -> id

-- | Collapse unicode character ranges if the general category doesn't make a
-- difference for width.
mergeRanges :: Eq b => [(a, b)] -> [(a, b)]
mergeRanges []  = []
mergeRanges [x] = [x]
mergeRanges (x@(_,xw):y@(_,yw):xs)
    | xw == yw  = mergeRanges (x:xs)
    | otherwise = x : mergeRanges (y:xs)


data UnicodeMap
    = Bin {-# UNPACK #-} !Char !UnicodeWidth !UnicodeMap !UnicodeMap
    | Tip

-- | Find the width of a unicode character
unicodeWidth :: UnicodeMap -> Char -> UnicodeWidth
unicodeWidth = goNothing
  where
    goNothing Tip !_ = Control
    goNothing (Bin kx x l r) k = case compare k kx of
        LT -> goNothing l k
        EQ -> x
        GT -> goJust r k kx x

    goJust Tip !_ !_ x' = x'
    goJust (Bin kx x l r) k kx' x' = case compare k kx of
        LT -> goJust l k kx' x'
        EQ -> x
        GT -> goJust r k kx x
{-# INLINABLE unicodeWidth #-}

-- | Convert a Map to a UnicodeMap for faster code.
repack :: M.Map Char UnicodeWidth -> UnicodeMap
repack MInt.Tip = Tip
repack (MInt.Bin _ k v l r) = Bin k v (repack l) (repack r)

-- | A list of Unicode ranges and the width assigned to them
unicodeSpec :: [(Char, UnicodeWidth)]
#include "unicodeWidth.inc"
