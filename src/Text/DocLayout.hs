{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
A prettyprinting library for the production of text documents,
including wrapped text, indentated blocks, and tables.
-}

module Text.DocLayout (
       Doc(..)
     , Dimensions(..)
     , Alignment(..)
     , NestingChange(..)
     , render
     , getDimensions
     , cr
     , blankline
     , blanklines
     , space
     , text
     , lit
--     , vfill
     , char
     , box
     , expandableBox
--     , prefixed
     , flush
     , nest
     , hang
--     , aligned
     , alignLeft
     , alignRight
     , alignCenter
     , nowrap
--     , withColumn
--     , withLineLength
--     , afterBreak
--     , offset
--     , minOffset
--     , height
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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.List.NonEmpty as N
import Data.String
import Data.List (foldl', intersperse)
import Control.Monad.RWS.Strict
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder (Builder)
import Data.String.Conversions (ConvertibleStrings(..), LazyText)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup)
#endif

import Debug.Trace

data Alignment = AlLeft | AlRight | AlCenter
  deriving (Show, Eq, Ord)

data NestingChange =
    IncreaseNesting !Int
  | SetNesting !Int
  deriving (Show, Eq, Ord)

data Doc
  = Empty
  | SoftBreak
  | LineBreak
  | HFill !Int
  | VFill !Int
  | Lit !Int !Text
  | PushNesting NestingChange
  | PopNesting
  | PushAlignment Alignment
  | PopAlignment
  | Box Bool !Int Doc
  | Concat Doc Doc
  deriving (Show, Eq, Ord)

instance Semigroup Doc where
  -- ensure that the leftmost element is accessible immediately
  (Concat w x) <> y = w <> (x <> y)
  x <> (Concat y z) =
      case x <> y of
        Concat{} -> Concat x (Concat y z)
        w        -> w <> z
  Lit n1 t1 <> Lit n2 t2 = Lit (n1 + n2) (t1 <> t2)
  LineBreak <> LineBreak = LineBreak
  SoftBreak <> SoftBreak = SoftBreak
  LineBreak <> SoftBreak = LineBreak
  SoftBreak <> LineBreak = LineBreak
  VFill m <> VFill n = VFill (max m n)
  LineBreak <> VFill m = VFill m
  VFill m <> LineBreak = VFill m
  x <> Empty = x
  Empty <> x = x
  x <> y = Concat x y

instance Monoid Doc where
  mappend = (<>)
  mempty  = Empty

instance IsString Doc where
  fromString = text

-- | Render a Doc with an optional width.
render :: ConvertibleStrings LazyText a => Maybe Int -> Doc -> a
render linelen = convertString . B.toLazyText . mconcat .
  intersperse (B.fromText "\n") .  map lineContents . snd . toLines linelen

data Line =
  Line
  { lineWidth    :: !Int
  , lineContents :: Builder
  } deriving (Show, Eq, Ord)

instance Semigroup Line where
  Line w1 c1 <> Line w2 c2 = Line (w1 + w2) (c1 <> c2)

instance Monoid Line where
  mappend = (<>)
  mempty = Line 0 mempty

data Dimensions =
  Dimensions
  { docWidth     :: !Int
  , docHeight    :: !Int
  } deriving (Show, Eq, Ord)

instance Semigroup Dimensions where
  Dimensions w1 h1 <> Dimensions w2 h2 =
    Dimensions (max w1 w2) (h1 + h2)

instance Monoid Dimensions where
  mappend = (<>)
  mempty = Dimensions 0 0

-- | Returns (width, height) of Doc.
getDimensions :: Maybe Int -> Doc -> Dimensions
getDimensions linelen = fst . toLines linelen

--
-- Constructors for Doc
--

-- | A literal string, possibly including newlines.
text :: String -> Doc
text s =
  case break (=='\n') s of
    ([], [])     -> mempty
    ([], (_:xs)) -> lit "" <> cr <> text xs
    (xs, [])     -> lit xs
    (xs, (_:ys)) -> lit xs <> cr <> text ys

-- | A raw string, assumed not to include newlines.
lit :: String -> Doc
lit s  = Lit (realLength s) (T.pack s)

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc
cr = LineBreak

-- | A breaking (reflowable) space.
space :: Doc
space = SoftBreak <> HFill 1

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
blankline :: Doc
blankline = VFill 1

-- | Inserts blank lines unless they exist already.
-- (@blanklines m <> blanklines n@ has the same effect as @blanklines (max m n)@.
blanklines :: Int -> Doc
blanklines n = VFill n

-- | Makes a 'Doc' flush against the left margin.
flush :: Doc -> Doc
flush doc = PushNesting (SetNesting 0) <> doc <> PopNesting

-- | Indents a 'Doc' by the specified number of spaces.
nest :: Int -> Doc -> Doc
nest ind doc = PushNesting (IncreaseNesting ind) <> doc <> PopNesting

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: Int -> Doc -> Doc -> Doc
hang ind start doc = start <> nest ind doc

-- | Concatenate a list of 'Doc's.
cat :: [Doc] -> Doc
cat = mconcat

-- | Same as 'cat'.
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate two 'Doc's, putting breakable space between them.
infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) x y = x <> space <> y

-- | Same as 'cat', but putting breakable spaces between the 'Doc's.
hsep :: [Doc] -> Doc
hsep = foldr (<+>) mempty

-- | Chomps trailing blank space off of a 'Doc'.
chomp :: Doc -> Doc
chomp d =
  case d of
    Empty -> Empty
    SoftBreak -> Empty
    LineBreak -> Empty
    HFill{} -> Empty
    VFill{} -> Empty
    Concat d1 d2 ->
      case chomp d2 of
        x | not (isPrintable x) -> chomp d1 <> x
        x -> d1 <> x
    _ -> d

-- | Remove leading blank lines.
nestle :: Doc -> Doc
nestle d =
  case d of
    VFill{} -> Empty
    Concat d1 d2 ->
      case nestle d1 of
        Empty -> nestle d2
        x -> x <> d2
    _ -> d

-- | Align left.
alignLeft :: Doc -> Doc
alignLeft doc =
  PushAlignment AlLeft <> cr <> doc <> cr <> PopAlignment

-- | Align right.
alignRight :: Doc -> Doc
alignRight doc =
  PushAlignment AlRight <> cr <> doc <> cr <> PopAlignment

-- | Align right.
alignCenter :: Doc -> Doc
alignCenter doc =
  PushAlignment AlCenter <> cr <> doc <> cr <> PopAlignment

-- | The empty document.
empty :: Doc
empty = mempty

-- | True if the document is empty.
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False


infixr 5 $$
-- | @a $$ b@ puts @a@ above @b@.
($$) :: Doc -> Doc -> Doc
($$) x y = x <> cr <> y

infixr 5 $+$
-- | @a $+$ b@ puts @a@ above @b@, with a blank line between.
($+$) :: Doc -> Doc -> Doc
($+$) x y = x <> blankline <> y

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- | List version of '$+$'.
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | A box with the specified width.  If content can't fit
-- in the width, it is silently truncated.
box :: Int -> Doc -> Doc
box n doc = Box False n doc

-- | An expandable box with the specified width.  If content can't fit
-- in the width, it is silently truncated.
expandableBox :: Int -> Doc -> Doc
expandableBox n doc = Box True n doc

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left. Also chomps
-- the document for backwards compatibility.
lblock :: Int -> Doc -> Doc
lblock w doc = box w (alignLeft $ chomp doc)

-- | Like 'lblock' but aligned to the right.
rblock :: Int -> Doc -> Doc
rblock w doc = box w (alignRight $ chomp doc)

-- | Like 'lblock' but aligned centered.
cblock :: Int -> Doc -> Doc
cblock w doc = box w (alignCenter $ chomp doc)

-- | Makes a 'Doc' non-reflowable.
nowrap :: Doc -> Doc
nowrap d =
  case d of
    SoftBreak -> Empty
    Concat d1 d2 -> nowrap d1 <> nowrap d2
    _ -> d

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: Doc -> Doc -> Doc -> Doc
inside start end contents =
  start <> contents <> end

-- | A character.
char :: Char -> Doc
char c = Lit (charWidth c) (T.singleton c)

-- | Puts a 'Doc' in curly braces.
braces :: Doc -> Doc
braces = inside (char '{') (char '}')

-- | Puts a 'Doc' in square brackets.
brackets :: Doc -> Doc
brackets = inside (char '[') (char ']')

-- | Puts a 'Doc' in parentheses.
parens :: Doc -> Doc
parens = inside (char '(') (char ')')

-- | Wraps a 'Doc' in single quotes.
quotes :: Doc -> Doc
quotes = inside (char '\'') (char '\'')

-- | Wraps a 'Doc' in double quotes.
doubleQuotes :: Doc -> Doc
doubleQuotes = inside (char '"') (char '"')


--
-- Code for dividing Doc into Lines (internal)
--

-- Divides Doc into Lines, and also returns doc dimensions (width, height).
toLines :: Maybe Int -> Doc -> (Dimensions, [Line])
toLines linelen doc = (dimensions, ls)
 where
   (ls, dimensions) = evalRWS (extractLines doc) linelen startingState
   startingState = RenderState
     { stColumn = 0
     , stLines  = []
     , stCurrent = Nothing
     , stNesting = N.fromList [0]
     , stAlignment = N.fromList [AlLeft]
     }

data RenderState =
  RenderState
  { stColumn      :: !Int
  , stLines       :: [Doc]
  , stCurrent     :: Maybe Doc
  , stNesting     :: N.NonEmpty Int
  , stAlignment   :: N.NonEmpty Alignment
  } deriving (Show)

type Renderer = RWS (Maybe Int) Dimensions RenderState

extractLines :: Doc -> Renderer [Line]
extractLines = fmap handleBoxes . reflowChunks . splitIntoChunks

handleBoxes :: [Doc] -> [Line]
handleBoxes = mconcat . map (mkLines False)
  where
  mkLines padRight d =
    case d of
      HFill n -> [Line 0 mempty] -- ignore final hfill
      Lit n t -> [Line n (B.fromText t)]
      Box expandable w d'
              -> let (dimensions, ls) = toLines (Just w) d'
                     trunc w' (Line _ b) = Line w'
                       (B.fromLazyText $ TL.take (fromIntegral w') $
                            B.toLazyText b)
                     expand w' (Line _ b) = Line w' b
                     adjust = case docWidth dimensions of
                                w' | w' <= w    -> id
                                w' | expandable -> map (expand w')
                                _               -> map (trunc w)
                     pad = if padRight
                              then map
                                   (\(Line lw b) ->
                                       if lw < w
                                          then Line w (b <>
                                            B.fromText (T.replicate (w - lw) " "))
                                          else Line lw b)
                              else id
                 in pad $ adjust ls
      Concat (HFill n) x -> combineLines padRight 0 0
        [Line n (B.fromText $ T.replicate n " ")] (mkLines padRight x)
      Concat d1 d2 -> combineLines padRight 0 0
                         (mkLines True d1) (mkLines False d2)
      _ -> [Line 0 mempty]

-- Combine two lists of lines, adding blank filler if needed.
combineLines :: Bool -> Int -> Int -> [Line] -> [Line] -> [Line]
combineLines _ _ _ [] [] = []
combineLines padRight xw yw [] (y:ys) =
  Line xw (B.fromText $ T.replicate xw " ") <> y :
    combineLines padRight xw (if yw == 0 then lineWidth y else yw) [] ys
combineLines padRight xw yw (x:xs) [] =
  x <> (if padRight
           then Line xw (B.fromText $ T.replicate xw " ")
           else Line 0 mempty) :
    combineLines padRight (if xw == 0 then lineWidth x else xw) yw xs []
combineLines padRight xw yw (x:xs) (y:ys) =
 x <> y : combineLines padRight
           (if xw == 0 then lineWidth x else xw)
           (if yw == 0 then lineWidth y else yw)
           xs ys

-- note, reversed lines...
splitIntoChunks :: Doc -> [Doc]
splitIntoChunks doc = reverse $
  case getChunk doc Nothing [] of
    (Nothing, ds) -> ds
    (Just d, ds)  -> d:ds

getChunk :: Doc -> Maybe Doc -> [Doc] -> (Maybe Doc, [Doc])
getChunk doc mbaccum ds =
  let ds' = maybe id (:) mbaccum $ ds
  in case doc of
    Empty -> (Nothing, ds')
    SoftBreak -> (Nothing, ds')
    LineBreak -> (Nothing, doc:ds')
    VFill{} -> (Nothing, doc:ds')
    PushNesting{} -> (Nothing, doc:ds')
    PopNesting{} -> (Nothing, doc:ds')
    PushAlignment{} -> (Nothing, doc:ds')
    PopAlignment{} -> (Nothing, doc:ds')
    Concat d1 d2 ->
      case getChunk d1 mbaccum ds of
        (mbdoc', ds'') -> getChunk d2 mbdoc' ds''
    _ -> (Just (maybe id (<>) mbaccum $ doc), ds)

reflowChunks :: [Doc] -> Renderer [Doc]
reflowChunks ds = do
  mapM processDoc ds
  current <- gets stCurrent
  ls <- gets stLines
  return $ dropWhile isEmpty $ reverse $ dropWhile isEmpty $
    case current of
      Just l  -> l:ls
      Nothing -> ls

widthOf :: Doc -> Int
widthOf d =
  case d of
    HFill n -> n
    Lit n _ -> n
    Box _ w _ -> w
    Concat d1 d2 -> widthOf d1 + widthOf d2
    _ -> 0

processDoc :: Doc -> Renderer ()
processDoc d = do
  col <- gets stColumn
  linelen <- ask
  mbcur <- gets stCurrent
  nesting <- N.head <$> gets stNesting
  alignment <- N.head <$> gets stAlignment
  let addAlignment doc =
        case linelen of
          Nothing  -> doc
          Just ll  ->
            case alignment of
              AlLeft   -> doc
              AlCenter -> HFill ((ll - widthOf doc) `div` 2) <> doc
              AlRight  -> HFill (ll - widthOf doc) <> doc
  case d of
    PushAlignment al ->
      modify $ \st -> st{ stAlignment = al N.<| stAlignment st }
    PopAlignment ->
      modify $ \st -> st{ stAlignment =
                          case N.uncons (stAlignment st) of
                            (_, Just l)  -> l
                            (_, Nothing) -> stAlignment st }
    PushNesting (IncreaseNesting n) ->
      modify $ \st ->
          st{ stNesting = N.head (stNesting st) + n N.<| stNesting st }
    PushNesting (SetNesting n) ->
      modify $ \st -> st{ stNesting = n N.<| stNesting st }
    PopNesting ->
      modify $ \st -> st{ stNesting =
                          case N.uncons (stNesting st) of
                            (_, Just l) -> l
                            (_, Nothing) -> stNesting st }
    LineBreak ->
       modify $ \st ->
         case mbcur of
           Nothing -> st
           Just cur ->
             st{ stLines = addAlignment cur : stLines st
               , stCurrent = Nothing
               , stColumn = 0 }
    VFill n ->
        modify $ \st ->
           st{ stLines = replicate n mempty ++
                         maybe id ((:) . addAlignment)
                           mbcur (stLines st)
             , stCurrent = Nothing
             , stColumn = 0 }
    _ | isPrintable d -> do
      let w = widthOf d
      case mbcur of
        Just cur
          | maybe False (< col + w) linelen -> do -- doesn't fit, create line
             let (d',w') = case d of
                             Concat (HFill n) x -> (x, w - n)
                             _ -> (d, w)
             modify $ \st ->
               st{ stLines = addAlignment cur : stLines st
                 , stCurrent = Just (HFill nesting <> d')
                 , stColumn = nesting + w' }
          | otherwise -> -- fits
             modify $ \st ->
               st{ stCurrent = Just (cur <> d)
                 , stColumn = col + w }
        Nothing ->  -- nothing yet on line
          modify $ \st ->
            st{ stLines = stLines st
              , stCurrent = Just (HFill nesting <> d)
              , stColumn = nesting + w }
    _ -> return ()

isPrintable :: Doc -> Bool
isPrintable Lit{} = True
isPrintable Box{} = True
isPrintable (Concat x y) = isPrintable x || isPrintable y
isPrintable _ = False

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
realLength :: String -> Int
realLength = foldl' (+) 0 . map charWidth
