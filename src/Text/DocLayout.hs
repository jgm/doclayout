{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{- |
   Module      : Text.DocLayout
   Copyright   : Copyright (C) 2010-2019 John MacFarlane
   License     : BSD 3

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A prettyprinting library for the production of text documents,
including wrapped text, indentated blocks, and tables.
-}

module Text.DocLayout (
       Doc(..)
     , HasChars(..)
     , render
     , cr
     , blankline
     , blanklines
     , space
     , text
     , char
     , prefixed
     , flush
     , nest
     , hang
     , beforeNonBlank
     , nowrap
     , afterBreak
     , offset
     , minOffset
     , height
     , lblock
     , cblock
     , rblock
     , (<>)
     , (<+>)
     , ($$)
     , ($+$)
     , isEmpty
     , empty
     , cat
     , hcat
     , hsep
     , vcat
     , vsep
     , nestle
     , chomp
     , inside
     , braces
     , brackets
     , parens
     , quotes
     , doubleQuotes
     , charWidth
     , realLength
     )

where
import Prelude
import Safe (lastMay, initSafe)
import Control.Monad
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (intersperse, foldl')
import Data.String
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.DocTemplates as DT
import Debug.Trace

-- | Class abstracting over various string types that
-- can fold over characters.
class (IsString a, Monoid a, Show a) => HasChars a where
  foldrChar     :: (Char -> b -> b) -> b -> a -> b
  splitLines    :: a -> [a]
  replicateChar :: Int -> Char -> a
  isNull        :: a -> Bool

instance HasChars Text where
  foldrChar         = T.foldr
  splitLines        = T.splitOn "\n"
  replicateChar n c = T.replicate n (T.singleton c)
  isNull            = T.null

instance HasChars String where
  foldrChar     = foldr
  splitLines    = lines . (++"\n")
  replicateChar = replicate
  isNull        = null

data Doc a = Text Int a
         | Block Int [a]
         | Prefixed Text (Doc a)
         | BeforeNonBlank (Doc a)
         | Flush (Doc a)
         | BreakingSpace
         | AfterBreak Text
         | CarriageReturn
         | NewLine
         | BlankLines Int  -- number of blank lines
         | Concat (Doc a) (Doc a)
         | Empty
         deriving (Show, Eq, Functor, Foldable, Traversable)

instance Semigroup (Doc a) where
  x <> Empty = x
  Empty <> x = x
  x <> y     = Concat x y

instance Monoid (Doc a) where
  mappend = (<>)
  mempty = Empty

instance IsString a => IsString (Doc a) where
  fromString = text

instance IsString a => DT.TemplateTarget (Doc a) where
  fromText = text . T.unpack
  removeFinalNewline = chomp
  nested = nest
  isEmpty = isEmpty

unfoldD :: Doc a -> [Doc a]
unfoldD Empty = []
unfoldD (Concat x@(Concat{}) y) = unfoldD x <> unfoldD y
unfoldD (Concat x y)            = x : unfoldD y
unfoldD x                       = [x]

isBlank :: Doc a -> Bool
isBlank BreakingSpace  = True
isBlank CarriageReturn = True
isBlank NewLine        = True
isBlank (BlankLines _) = True
-- isBlank (Text _ (c:_)) = isSpace c
isBlank _              = False

-- | True if the document is empty.
isEmpty :: Doc a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | The empty document.
empty :: Doc a
empty = mempty

-- | Concatenate a list of 'Doc's.
cat :: [Doc a] -> Doc a
cat = mconcat

-- | Same as 'cat'.
hcat :: [Doc a] -> Doc a
hcat = mconcat

-- | Concatenate a list of 'D's, putting breakable spaces
-- between them.
infixr 6 <+>
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> space <> y

-- | Same as 'cat', but putting breakable spaces between the
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

type DocState a = State (RenderState a) ()

data RenderState a = RenderState{
         output     :: [a]        -- ^ In reverse order
       , prefix     :: Text
       , usePrefix  :: Bool
       , lineLength :: Maybe Int  -- ^ 'Nothing' means no wrapping
       , column     :: Int
       , newlines   :: Int        -- ^ Number of preceding newlines
       }

outp :: HasChars a => Int -> a -> DocState a
outp off s | off < 0 = do  -- offset < 0 means newline characters
  st' <- get
  let rawpref = prefix st'
  when (column st' == 0 && usePrefix st' && not (T.null rawpref)) $ do
    let pref = fromString $ T.unpack $ T.dropWhileEnd isSpace rawpref
    modify $ \st -> st{ output = pref : output st
                      , column = column st + realLength pref }
  modify $ \st -> st { output = s : output st
                     , column = 0
                     }
outp off s = do           -- offset >= 0 (0 might be combining char)
  st' <- get
  let pref = fromString $ T.unpack $ prefix st'
  when (column st' == 0 && usePrefix st' && not (isNull pref)) $
    modify $ \st -> st{ output = pref : output st
                    , column = column st + realLength pref }
  modify $ \st -> st{ output = s : output st
                    , column = column st + off
                    , newlines = 0 }

-- | Renders a 'Doc'.  @render (Just n)@ will use
-- a line length of @n@ to reflow text on breakable spaces.
-- @render Nothing@ will not reflow text.
render :: HasChars a => Maybe Int -> Doc a -> a
render linelen doc = mconcat . reverse . output $
  execState (renderDoc doc) startingState
   where startingState = RenderState{
                            output = mempty
                          , prefix = mempty
                          , usePrefix = True
                          , lineLength = linelen
                          , column = 0
                          , newlines = 2 }

renderDoc :: HasChars a => Doc a -> DocState a
renderDoc = renderList . traceShowId . unfoldD

data IsBlock a = IsBlock Int [a]

-- This would be nicer with a pattern synonym
-- pattern VBlock i s <- mkIsBlock -> Just (IsBlock ..)

renderList :: HasChars a => [Doc a] -> DocState a
renderList [] = return ()

renderList (Concat{} : xs) = renderList xs -- should not happen after unfoldD

renderList (Empty : xs) = renderList xs -- should not happen after unfoldD

renderList (NewLine : []) = renderList [CarriageReturn]

renderList (BlankLines _ : []) = renderList [CarriageReturn]

renderList (BreakingSpace : []) = return ()

renderList (Text off s : xs) = do
  outp off s
  renderList xs

renderList (Prefixed pref d : xs) = do
  st <- get
  let oldPref = prefix st
  put st{ prefix = prefix st <> pref }
  renderDoc (d <> CarriageReturn)
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

renderList (Flush d : xs) = do
  st <- get
  let oldUsePrefix = usePrefix st
  put st{ usePrefix = False }
  renderDoc d
  modify $ \s -> s{ usePrefix = oldUsePrefix }
  renderList xs

renderList (BeforeNonBlank d : xs) =
  case xs of
    (x:_) | isBlank x -> renderList xs
          | otherwise -> renderDoc d >> renderList xs
    []                -> renderList xs

renderList (BlankLines m : BlankLines n : xs) =
  renderList (BlankLines (max m n) : xs)

renderList (BlankLines num : BreakingSpace : xs) =
  renderList (BlankLines num : xs)

renderList (BlankLines m : CarriageReturn : xs) =
  renderList (BlankLines m : xs)

renderList (BlankLines m : NewLine : xs) =
  renderList (BlankLines m : xs)

renderList (NewLine : BlankLines m : xs) =
  renderList (BlankLines m : xs)

renderList (NewLine : BreakingSpace : xs) =
  renderList (NewLine : xs)

renderList (NewLine : CarriageReturn : xs) =
  renderList (NewLine : xs)

renderList (CarriageReturn : CarriageReturn : xs) =
  renderList (CarriageReturn : xs)

renderList (CarriageReturn : NewLine : xs) =
  renderList (NewLine : xs)

renderList (CarriageReturn : BlankLines m : xs) =
  renderList (BlankLines m : xs)

renderList (CarriageReturn : BreakingSpace : xs) =
  renderList (CarriageReturn : xs)

renderList (BlankLines num : xs) = do
  st <- get
  case output st of
     _ | newlines st > num -> return ()
       | otherwise -> replicateM_ (1 + num - newlines st)
                        (do outp (-1) "\n"
                            modify $ \st' -> st'{ newlines = newlines st' + 1 })
  renderList xs

renderList (CarriageReturn : xs) = do
  st <- get
  if newlines st > 0 || null xs
     then renderList xs
     else do
       outp (-1) "\n"
       renderList xs

renderList (NewLine : xs) = do
  outp (-1) "\n"
  renderList xs

renderList (BreakingSpace : CarriageReturn : xs) =
  renderList (CarriageReturn:xs)
renderList (BreakingSpace : NewLine : xs) = renderList (NewLine:xs)
renderList (BreakingSpace : BlankLines n : xs) = renderList (BlankLines n:xs)
renderList (BreakingSpace : BreakingSpace : xs) = renderList (BreakingSpace:xs)
renderList (BreakingSpace : xs) = do
  let isText (Text _ _)     = True
      isText (Block _ _)    = True
      isText (AfterBreak _) = True
      isText _              = False
  let isBreakingSpace BreakingSpace = True
      isBreakingSpace _             = False
  let xs' = dropWhile isBreakingSpace xs
  let next = takeWhile isText xs'
  st <- get
  let off = foldl' (+) 0 $ map offsetOf next
  case lineLength st of
        Just l | column st + 1 + off > l -> do
          outp (-1) "\n"
          renderList xs'
        _  -> do
          outp 1 " "
          renderList xs'

renderList (AfterBreak t : xs) = do
  st <- get
  if newlines st > 0
     then renderList (fromString (T.unpack t) : xs)
     else renderList xs

renderList (Block i1 s1 : Block i2 s2  : xs) =
  renderList (mergeBlocks False (IsBlock i1 s1) (IsBlock i2 s2) : xs)

renderList (Block i1 s1 : BreakingSpace : Block i2 s2 : xs) =
  renderList (mergeBlocks True (IsBlock i1 s1) (IsBlock i2 s2) : xs)

renderList (Block _width lns : xs) = do
  st <- get
  let oldPref = prefix st
  case column st - realLength oldPref of
        n | n > 0 -> modify $ \s -> s{ prefix = oldPref <> T.replicate n " " }
        _ -> return ()
  renderList $ intersperse CarriageReturn (map (Text 0) lns)
  modify $ \s -> s{ prefix = oldPref }
  renderList xs

mergeBlocks :: HasChars a => Bool -> IsBlock a -> IsBlock a -> Doc a
mergeBlocks addSpace (IsBlock w1 lns1) (IsBlock w2 lns2) =
  Block (w1 + w2 + if addSpace then 1 else 0) $
     zipWith (\l1 l2 -> pad w1 l1 <> l2) lns1' (map sp lns2')
    where (lns1', lns2') = case (length lns1, length lns2) of
                                (x, y) | x > y -> (lns1,
                                                   lns2 ++ replicate (x - y)
                                                            mempty)
                                       | x < y -> (lns1 ++ replicate (y - x)
                                                            mempty ,
                                                   lns2)
                                       | otherwise -> (lns1, lns2)
          pad n s = s <> replicateChar (n - realLength s) ' '
          sp xs = if addSpace && realLength xs > 0
                     then " " <> xs
                     else xs

offsetOf :: Doc a -> Int
offsetOf (Text o _)    = o
offsetOf (Block w _)   = w
offsetOf BreakingSpace = 1
offsetOf _             = 0

-- | A literal string.
text :: IsString a => String -> Doc a
text "" = mempty
text s = case break (=='\n') s of
           ("", "")   -> Empty
           (xs, "")   -> Text (realLength xs) (fromString xs)
           ("", _:ys) -> NewLine <> text ys
           (xs, _:ys) -> Text (realLength xs) (fromString xs) <>
                           NewLine <> text ys

-- | A character.
char :: IsString a => Char -> Doc a
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
blanklines n = BlankLines n

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
hang ind start doc
  | isEmpty doc = Empty
  | otherwise   = start <> nest ind doc

-- | @beforeNonBlank d@ conditionally includes @d@ unless it is
-- followed by blank space.
beforeNonBlank :: Doc a -> Doc a
beforeNonBlank d = BeforeNonBlank d

-- | Makes a 'Doc' non-reflowable.
nowrap :: IsString a => Doc a -> Doc a
nowrap doc = mconcat . map replaceSpace . unfoldD $ doc
  where replaceSpace BreakingSpace = Text 1 $ fromString " "
        replaceSpace x             = x

-- | Content to print only if it comes at the beginning of a line,
-- to be used e.g. for escaping line-initial `.` in roff man.
afterBreak :: Text -> Doc a
afterBreak t = AfterBreak t

-- | Returns the width of a 'Doc'.
offset :: HasChars a => Doc a -> Int
offset d = maximum (0: map realLength (splitLines $ render Nothing d))

-- | Returns the minimal width of a 'Doc' when reflowed at breakable spaces.
minOffset :: HasChars a => Doc a -> Int
minOffset d = maximum (0: map realLength (splitLines $ render (Just 0) d))

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

block :: HasChars a => (a -> a) -> Int -> Doc a -> Doc a
block filler width d
  | width < 1 && not (isEmpty d) = block filler 1 d
  | otherwise                    = Block width $ map filler
                                 $ chop width $ render (Just width) d

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
                                   (len' + clen, l' <> cs):rest
                             [] -> [(clen, cs)]) [] l

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: Doc a -> Doc a -> Doc a -> Doc a
inside start end contents =
  start <> contents <> end

-- | Puts a 'Doc' in curly braces.
braces :: IsString a => Doc a -> Doc a
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: IsString a => Doc a -> Doc a
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: IsString a => Doc a -> Doc a
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: IsString a => Doc a -> Doc a
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: IsString a => Doc a -> Doc a
doubleQuotes = inside (char '"') (char '"')

-- | Returns width of a character in a monospace font:  0 for a combining
-- character, 1 for a regular character, 2 for an East Asian wide character.
charWidth :: Char -> Int
charWidth c =
  case c of
      _ | c <  '\x0300'                    -> 1
        | c >= '\x0300' && c <= '\x036F'   -> 0  -- combining
        | c >= '\x0370' && c <= '\x10FC'   -> 1
        | c >= '\x1100' && c <= '\x115F'   -> 2
        | c >= '\x1160' && c <= '\x11A2'   -> 1
        | c >= '\x11A3' && c <= '\x11A7'   -> 2
        | c >= '\x11A8' && c <= '\x11F9'   -> 1
        | c >= '\x11FA' && c <= '\x11FF'   -> 2
        | c >= '\x1200' && c <= '\x2328'   -> 1
        | c >= '\x2329' && c <= '\x232A'   -> 2
        | c >= '\x232B' && c <= '\x2E31'   -> 1
        | c >= '\x2E80' && c <= '\x303E'   -> 2
        | c == '\x303F'                    -> 1
        | c >= '\x3041' && c <= '\x3247'   -> 2
        | c >= '\x3248' && c <= '\x324F'   -> 1 -- ambiguous
        | c >= '\x3250' && c <= '\x4DBF'   -> 2
        | c >= '\x4DC0' && c <= '\x4DFF'   -> 1
        | c >= '\x4E00' && c <= '\xA4C6'   -> 2
        | c >= '\xA4D0' && c <= '\xA95F'   -> 1
        | c >= '\xA960' && c <= '\xA97C'   -> 2
        | c >= '\xA980' && c <= '\xABF9'   -> 1
        | c >= '\xAC00' && c <= '\xD7FB'   -> 2
        | c >= '\xD800' && c <= '\xDFFF'   -> 1
        | c >= '\xE000' && c <= '\xF8FF'   -> 1 -- ambiguous
        | c >= '\xF900' && c <= '\xFAFF'   -> 2
        | c >= '\xFB00' && c <= '\xFDFD'   -> 1
        | c >= '\xFE00' && c <= '\xFE0F'   -> 1 -- ambiguous
        | c >= '\xFE10' && c <= '\xFE19'   -> 2
        | c >= '\xFE20' && c <= '\xFE26'   -> 1
        | c >= '\xFE30' && c <= '\xFE6B'   -> 2
        | c >= '\xFE70' && c <= '\xFEFF'   -> 1
        | c >= '\xFF01' && c <= '\xFF60'   -> 2
        | c >= '\xFF61' && c <= '\x16A38'  -> 1
        | c >= '\x1B000' && c <= '\x1B001' -> 2
        | c >= '\x1D000' && c <= '\x1F1FF' -> 1
        | c >= '\x1F200' && c <= '\x1F251' -> 2
        | c >= '\x1F300' && c <= '\x1F773' -> 1
        | c >= '\x20000' && c <= '\x3FFFD' -> 2
        | otherwise                        -> 1

-- | Get real length of string, taking into account combining and double-wide
-- characters.
realLength :: HasChars a => a -> Int
realLength = foldrChar (\c tot -> tot + charWidth c) 0

