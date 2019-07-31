{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
A prettyprinting library for the production of text documents,
including wrapped text, indentated blocks, and tables.
-}

module Text.DocLayout (
       Doc
     , render
     , getDimensions
     , cr
     , blankline
     , blanklines
     , space
     , text
     , vfill
     , char
     , box
     , prefixed
     , flush
     , nest
     , aligned
     , hang
     , alignLeft
     , alignRight
     , alignCenter
     , nowrap
     , withColumn
     , withLineLength
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.List.NonEmpty as N
import Control.Monad (unless)
import Data.String
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.List (foldl', transpose, intersperse)
import Control.Monad.State.Strict
import qualified Data.Text.Lazy.Builder as B
import Data.Text.Lazy.Builder (Builder)
import Data.Foldable (toList)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Semigroup (Semigroup)
#endif

newtype Doc = Doc{ unDoc :: Seq D }
  deriving (Semigroup, Monoid, Show)

data D =
    Text !Int !Text      -- ^ text with real width, does not break, no newline
  | VFill !Int !Text     -- ^ like text, but repeats to fill vertically
                         -- when adjacent to a block
  | Newline              -- ^ newline
  | SoftSpace            -- ^ space or newline depending on context
  | PushNesting (Int -> Int -> Int)
                         -- ^ change nesting level: first argument to
                         -- function is current column (before left
                         -- padding from centering or right alignment
                         -- is added), second is current nesting level.
  | PopNesting           -- ^ restore previous nesting level
  | Blanks !Int          -- ^ ensure that there are at least n blank lines
                         -- but do not add additional ones if there are
  | Box !Int Doc         -- ^ lay out the document with the given width,
                         -- and treat it as an indivisible unit
  | WithColumn (Int -> Doc) -- ^ output conditional on column number
  | WithLineLength (Maybe Int -> Doc) -- ^ output conditional on line length
  | PushAlignment Alignment -- ^ set alignment
  | PopAlignment        -- ^ revert to previous alignment

instance Show D where
  show (Text n s) = "Text " ++ show n ++ " " ++ show s
  show (VFill n s) = "VFill " ++ show n ++ " " ++ show s
  show Newline = "Newline"
  show SoftSpace = "SoftSpace"
  show (PushNesting _) = "PushNesting <function>"
  show PopNesting = "PopNesting"
  show (Blanks n) = "Blanks " ++ show n
  show (Box n d) = "Box " ++ show n ++ " " ++ show d
  show (WithColumn _) = "WithColumn <function>"
  show (WithLineLength _) = "WithLineLength <function>"
  show (PushAlignment al) = "PushAlignment " ++ show al
  show PopAlignment = "PopAlignment"

data Alignment = AlLeft | AlRight | AlCenter
  deriving (Show)

newtype Line = Line { unLine :: [D] }
  deriving (Show)

instance IsString Doc where
  fromString = text

data RenderState = RenderState{
         column      :: !Int
       , nesting     :: N.NonEmpty Int
       , alignment   :: N.NonEmpty Alignment
       , lineLength  :: Maybe Int  -- ^ 'Nothing' means no wrapping
       , blanks      :: !Int       -- ^ Number of preceding blank lines
       , currentLine :: [D]
       , actualWidth :: Int        -- ^ Actual max line width
       }
  deriving (Show)

-- | Render a Doc with an optional width.
render :: Maybe Int -> Doc -> Text
render linelen = TL.toStrict . B.toLazyText . mconcat .
                 intersperse (B.singleton '\n') .
                 map buildLine .  snd .  buildLines linelen

-- | Returns (width, height) of Doc.
getDimensions :: Maybe Int -> Doc -> (Int, Int)
getDimensions linelen doc = (w, length ls)  -- width x height
  where
   (w, ls) = buildLines linelen doc

buildLines :: Maybe Int -> Doc -> (Int, [Line])
buildLines linelen doc =
  evalState (do ls <- groupLines (toList (unDoc doc))
                w <- gets actualWidth
                return (w, handleBoxes ls))
    (startingState linelen)

startingState :: Maybe Int -> RenderState
startingState linelen =
  RenderState{ column = 0
             , nesting = N.fromList [0]
             , alignment = N.fromList [AlLeft]
             , lineLength = linelen
             , blanks = 0
             , currentLine = mempty
             , actualWidth = 0
             }


-- Group Ds into lines.
groupLines :: [D] -> State RenderState [Line]
groupLines [] = ($ []) <$> emitLine
groupLines (d:ds) = do
  linelen <- gets lineLength
  col <- gets column
  curline <- gets currentLine
  let hasSoftSpace = case curline of
                           SoftSpace:_ -> True
                           _           -> False
  case d of
    WithColumn f -> groupLines $ toList (unDoc (f col)) <> ds
    WithLineLength f -> groupLines $ toList (unDoc (f linelen)) <> ds
    PushNesting f -> do
      modify $ \st ->
        st{ nesting = f col (N.head (nesting st)) N.<| nesting st }
      groupLines ds
    PopNesting -> do
      modify $ \st -> st{ nesting = fromMaybe (nesting st)
                              (snd $ N.uncons (nesting st)) }
      groupLines ds
    PushAlignment align' -> do
      modify $ \st -> st{ alignment = align' N.<| alignment st }
      groupLines ds
    PopAlignment -> do
      modify $ \st -> st{ alignment = fromMaybe (alignment st)
                            (snd $ N.uncons (alignment st)) }
      groupLines ds
    SoftSpace -> do
      unless hasSoftSpace $
        modify $ \st -> st{ currentLine = d : currentLine st
                          , column = column st + 1 }
      groupLines ds
    Blanks n -> do
      f <- emitLine
      g <- emitBlanks n
      f . g <$> groupLines ds
    Text len _
      | maybe True ((col + len) <=) linelen || not hasSoftSpace -> do
          addToCurrentLine d
          groupLines ds
      | otherwise -> do
          f <- emitLine
          f <$> groupLines (d:ds)
    VFill len _
      | maybe True ((col + len) <=) linelen || not hasSoftSpace -> do
          addToCurrentLine d
          groupLines ds
      | otherwise -> do
          f <- emitLine
          f <$> groupLines (d:ds)
    Box len _doc
      | maybe True ((col + len) <=) linelen || not hasSoftSpace -> do
          addToCurrentLine d
          groupLines ds
      | otherwise -> do
          f <- emitLine
          f <$> groupLines (d:ds)
    Newline -> do
          f <- emitLine
          f <$> groupLines ds

addToCurrentLine :: D -> State RenderState ()
addToCurrentLine d = do
  curline <- gets currentLine
  nest' N.:| _ <- gets nesting
  let curline' =
        case d of
          SoftSpace -> curline
          _ | null curline ->
                 [Text nest' (T.replicate nest' " ")]
            | otherwise -> curline
  modify $ \st -> st{ currentLine = d : curline'
                    , column = column st + dLength d }

emitLine :: State RenderState ([Line] -> [Line])
emitLine = do
  align' N.:| _ <- gets alignment
  curline <- gets currentLine
  col <- gets column
  nest' N.:| _ <- gets nesting
  modify $ \st -> st{ currentLine = []
                    , column = nest'
                    , actualWidth = col
                    }
  if all isSoftSpace curline
     then return id
     else do
       modify $ \st -> st{ blanks = 0 }
       let printable = reverse (dropWhile isSoftSpace curline)
       let printableWidth = foldr ((+) . dLength) 0 printable
       mbLineLength <- gets lineLength
       let pad =
            case (align', mbLineLength, printableWidth) of
              (AlRight, Just linelen, w) | w > 0
                 -> let padw = linelen - w
                    in  (Text padw (T.replicate padw " ") :)
              (AlCenter, Just linelen, w) | w > 0
                 -> let padw = (linelen - w) `div` 2
                    in  (Text padw (T.replicate padw " ") :) .
                        (++ (replicate padw SoftSpace))
              _                           -> id
       return (Line (pad printable) :)

emitBlanks :: Int -> State RenderState ([Line] -> [Line])
emitBlanks n = do
  nest' N.:| _ <- gets nesting
  bls <- gets blanks
  let blsNeeded = n - bls
  if blsNeeded > 0
     then do
       modify $ \st -> st { currentLine = []
                          , column = nest'
                          , blanks = bls + 1 }
       ((Line []:) .) <$> emitBlanks n
     else return id

isSoftSpace :: D -> Bool
isSoftSpace SoftSpace = True
isSoftSpace _ = False

isBox :: D -> Bool
isBox Box{} = True
isBox _ = False

handleBoxes :: [Line] -> [Line]
handleBoxes [] = []
handleBoxes (Line ds : ls)
  | any isBox ds = newlines ++ handleBoxes ls
  | otherwise    = Line ds : handleBoxes ls
 where
  newlines = map (Line . mconcat) $ transpose $
              zipWith padBox boxes [1..]
  boxes :: [(Int, Int, [[D]])]
  boxes = map expandBox ds
  numboxes = length boxes
  expandBox (Box w doc) = (actualw, length ls'', ls'')
    where (actualw, ls') = buildLines (Just w) doc
          ls'' = map unLine ls'
  expandBox d = (dLength d, 1, [[d]])
  maxdepth = maximum $ map (\(_,x,_) -> x) boxes
  padBox (w, d, ls') num
    | d < maxdepth = ls' ++ replicate (maxdepth - d)
                             (case ls' of
                               [[VFill _ t]] -> [VFill w t]
                               _ | num == numboxes -> []
                                 | otherwise -> [Text w (T.replicate w " ")])
    | otherwise    = ls'

dLength :: D -> Int
dLength (Text n _) = n
dLength SoftSpace  = 1
dLength (Box n _)  = n
dLength _          = 0

-- Render a line.
buildLine :: Line -> Builder
buildLine (Line ds) = mconcat (map buildD $ dropTrailingSoftSpaces ds)
 where
   buildD (Text _ t) = B.fromText t
   buildD (VFill _ t) = B.fromText t
   buildD SoftSpace  = B.fromText " "
   buildD _ = mempty
   dropTrailingSoftSpaces = reverse .  dropWhile isSoftSpace . reverse

single :: D -> Doc
single = Doc . Seq.singleton

-- | A breaking (reflowable) space.
space :: Doc
space = single SoftSpace

-- | A carriage return.  Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc
cr = single Newline

-- | Inserts a blank line unless one exists already.
-- (@blankline <> blankline@ has the same effect as @blankline@.
blankline :: Doc
blankline = single (Blanks 1)

-- | Inserts blank lines unless they exist already.
-- (@blanklines m <> blanklines n@ has the same effect as @blanklines (max m n)@.
blanklines :: Int -> Doc
blanklines n = single (Blanks n)

-- | A literal string.
text :: String -> Doc
text = Doc . toChunks
  where toChunks :: String -> Seq D
        toChunks [] = mempty
        toChunks s =
           case break (=='\n') s of
             ([], _:ys) -> Newline Seq.<| toChunks ys
             (xs, _:ys) -> Text (realLength xs) (T.pack xs) Seq.<|
                               (Newline Seq.<| toChunks ys)
             (xs, [])   -> Seq.singleton $ Text (realLength xs) (T.pack xs)

-- | A string that fills vertically next to a block; may not
-- contain \n.
vfill :: String -> Doc
vfill xs = single (VFill (realLength xs) (T.pack xs))

-- | A character.
char :: Char -> Doc
char c = single $ Text (charWidth c) (T.singleton c)

-- | Returns the width of a 'Doc' (without reflowing).
offset :: Doc -> Int
offset doc = fst $ getDimensions Nothing doc

-- | Returns the height of a block or other 'Doc' (without reflowing).
height :: Doc -> Int
height doc = snd $ getDimensions Nothing doc

-- | Returns the minimal width of a 'Doc' when reflowed at breakable spaces.
minOffset :: Doc -> Int
minOffset doc = fst $ getDimensions (Just 0) doc

-- | Output conditional on current column.
withColumn :: (Int -> Doc) -> Doc
withColumn f = single (WithColumn f)

-- | Output conditional on line length.
withLineLength :: (Maybe Int -> Doc) -> Doc
withLineLength f = single (WithLineLength f)

-- | Content to print only if it comes at the beginning of a line,
-- to be used e.g. for escaping line-initial `.` in roff man.
afterBreak :: String -> Doc
afterBreak s =
  withColumn (\c -> if c == 0
                       then text s
                       else mempty)

-- | A box with the specified width.
box :: Int -> Doc -> Doc
box n doc = single $ Box n doc

-- | @lblock n d@ is a block of width @n@ characters, with
-- text derived from @d@ and aligned to the left.
lblock :: Int -> Doc -> Doc
lblock w doc = box w (alignLeft doc)

-- | Like 'lblock' but aligned to the right.
rblock :: Int -> Doc -> Doc
rblock w doc = box w (alignRight doc)

-- | Like 'lblock' but aligned centered.
cblock :: Int -> Doc -> Doc
cblock w doc = box w (alignCenter doc)

-- | Align left.
alignLeft :: Doc -> Doc
alignLeft doc =
  single (PushAlignment AlLeft) <>
  doc <>
  single PopAlignment

-- | Align right.
alignRight :: Doc -> Doc
alignRight doc =
  single (PushAlignment AlRight) <>
  doc <>
  single PopAlignment

-- | Align right.
alignCenter :: Doc -> Doc
alignCenter doc =
  single (PushAlignment AlCenter) <>
  doc <>
  single PopAlignment

-- | Chomps trailing blank space off of a 'Doc'.
chomp :: Doc -> Doc
chomp (Doc ds) =
  case Seq.viewr ds of
    rest Seq.:> Newline   -> chomp (Doc rest)
    rest Seq.:> Blanks _  -> chomp (Doc rest)
    rest Seq.:> SoftSpace -> chomp (Doc rest)
    _                     -> Doc ds

-- | Removes leading blank lines from a 'Doc'.
nestle :: Doc -> Doc
nestle (Doc ds) =
  case Seq.viewl ds of
    Newline    Seq.:< rest  -> nestle (Doc rest)
    Blanks _ Seq.:< rest    -> nestle (Doc rest)
    _                       -> Doc ds

-- | True if the document is empty.  A document with
-- non-printing directives like 'PopNesting' counts as
-- empty.  So @isEmpty (nest 5 empty) == True@.
isEmpty :: Doc -> Bool
isEmpty = all isDirective . unDoc
  where
    isDirective PushNesting{} = True
    isDirective PopNesting = True
    isDirective WithColumn{} = True
    isDirective WithLineLength{} = True
    isDirective PushAlignment{} = True
    isDirective PopAlignment = True
    isDirective _ = False

-- | The empty document.
empty :: Doc
empty = mempty

-- | Concatenate a list of 'Doc's.
cat :: [Doc] -> Doc
cat = mconcat

-- | Same as 'cat'.
hcat :: [Doc] -> Doc
hcat = mconcat

-- | Concatenate two 'Doc's, putting breakable space between them.
infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> space <> y

-- | Same as 'cat', but putting breakable spaces between the 'Doc's.
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

infixr 5 $$
-- | @a $$ b@ puts @a@ above @b@.
($$) :: Doc -> Doc -> Doc
($$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> cr <> y

infixr 5 $+$
-- | @a $+$ b@ puts @a@ above @b@, with a blank line between.
($+$) :: Doc -> Doc -> Doc
($+$) x y
  | isEmpty x = y
  | isEmpty y = x
  | otherwise = x <> blankline <> y

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- | List version of '$+$'.
vsep :: [Doc] -> Doc
vsep = foldr ($+$) empty

-- | Makes a 'Doc' non-reflowable.
nowrap :: Doc -> Doc
nowrap (Doc ds) = Doc $ fmap replaceSpace ds
  where replaceSpace SoftSpace = Text 1 " "
        replaceSpace x         = x

-- | Makes a 'Doc' flush against the left margin.
flush :: Doc -> Doc
flush doc =
  single (PushNesting (\_ _ -> 0)) <> doc <> single PopNesting

-- | Indents a 'Doc' by the specified number of spaces.
nest :: Int -> Doc -> Doc
nest ind doc =
  single (PushNesting (\_ n -> n + ind)) <> doc <> single PopNesting

-- | A hanging indent. @hang ind start doc@ prints @start@,
-- then @doc@, leaving an indent of @ind@ spaces on every
-- line but the first.
hang :: Int -> Doc -> Doc -> Doc
hang ind start doc = start <> nest ind doc

-- | Align a 'Doc' so that new lines start at current column.
aligned :: Doc -> Doc
aligned doc =
  single (PushNesting (\col _ -> col)) <>
  doc <>
  single PopNesting

-- | Uses the specified string as a prefix for every line of
-- the argument.
prefixed :: String -> Doc -> Doc
prefixed pref doc =
  withLineLength $ \mblen ->
    let boxwidth =
         case mblen of
           Just l  -> l - realLength pref
           Nothing -> fst (getDimensions Nothing doc) - realLength pref
    in  vfill pref <> box boxwidth doc

-- | Encloses a 'Doc' inside a start and end 'Doc'.
inside :: Doc -> Doc -> Doc -> Doc
inside start end contents =
  start <> contents <> end

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
