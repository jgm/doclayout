{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.DocLayout.ANSIFont
  ( Font(..)
  , baseFont
  , StyleReq(..)
  , Weight(..)
  , Shape(..)
  , Color8(..)
  , Underline(..)
  , Foreground(..)
  , Background(..)
  , (~>)
  , renderFont
  ) where

import Data.Data (Data)
import Data.String

data Font = Font
  { ftWeight :: Weight,
    ftShape :: Shape,
    ftUnderline :: Underline,
    ftForeground :: Foreground,
    ftBackground :: Background
  }
  deriving (Show, Eq, Read, Data, Ord)

baseFont :: Font
baseFont = Font Normal Roman ULNone FGDefault BGDefault

data Weight = Normal | Bold deriving (Show, Eq, Read, Data, Ord)
data Shape = Roman | Italic deriving (Show, Eq, Read, Data, Ord)
data Color8 = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Show, Eq, Enum, Read, Data, Ord)
data Underline = ULNone | ULSingle | ULDouble | ULCurly deriving (Show, Eq, Read, Data, Ord)
data Foreground = FGDefault | FG Color8 deriving (Show, Eq, Read, Data, Ord)
data Background = BGDefault | BG Color8 deriving (Show, Eq, Read, Data, Ord)

data StyleReq =
  RWeight Weight | RShape Shape | RForeground Foreground | RBackground Background | RUnderline Underline
  deriving (Show, Eq, Read, Data, Ord)

(~>) :: Font -> StyleReq -> Font
(~>) f (RWeight w) = f{ftWeight = w}
(~>) f (RShape s) = f{ftShape = s}
(~>) f (RForeground c) = f{ftForeground = c}
(~>) f (RBackground c) = f{ftBackground = c}
(~>) f (RUnderline u) = f{ftUnderline = u}

rawSGR :: (Semigroup a, IsString a) => a -> a
rawSGR n = "\ESC[" <> n <> "m"

class SGR b where
  renderSGR :: (Semigroup a, IsString a) => b -> a

instance SGR Weight where
  renderSGR Normal = rawSGR "22"
  renderSGR Bold = rawSGR "1"

instance SGR Shape where
  renderSGR Roman = rawSGR "23"
  renderSGR Italic = rawSGR "3"

instance SGR Foreground where
  renderSGR FGDefault = rawSGR "39"
  renderSGR (FG a) = (rawSGR . fromString . show . (+) 30 . fromEnum) a

instance SGR Background where
  renderSGR BGDefault = rawSGR "49"
  renderSGR (BG a) = (rawSGR . fromString . show . (+) 40 . fromEnum) a

instance SGR Underline where
  renderSGR ULNone = rawSGR "24"
  renderSGR ULSingle = rawSGR "4"
  renderSGR ULDouble = rawSGR "21"
  renderSGR ULCurly = rawSGR "4:3"

renderFont :: (Semigroup a, IsString a) => Font -> a
renderFont f
  | f == baseFont = rawSGR "0"
  | otherwise =
      renderSGR (ftWeight f)
        <> renderSGR (ftShape f)
        <> renderSGR (ftForeground f)
        <> renderSGR (ftBackground f)
        <> renderSGR (ftUnderline f)
