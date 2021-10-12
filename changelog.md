# doclayout

## 0.3.1.1

  * Fix the end of the block of zero width characters which contains
    the zero-width joiners and directional markings (Stephen Morgan, #5).
    This fixes a regression introduced in 0.3.1, affecting code
    points 0x2010 to 0x2030.

## 0.3.1

  * Improved handling of emojis.  Emojis are double-wide, but
    previously this library did not treat them as such.  We now
    have comprehensive support of emojis, including variation
    modifiers and zero-width joiners, verified by a test suite.
    Performance has been confirmed to be no worse for text without emojis.
    (Stephen Morgan, #1).  API changes: export `realLengthNoShortcut`,
    `isEmojiModifier`, `isEmojiVariation`, `isEmojiJoiner`.

## 0.3.0.2

 * NOINLINE `literal` instead of `fromString` (#2, sjakobi).
   This produces a further reduction in allocations and
   pandoc compile time.

## 0.3.0.1

 * NOINLINE `fromString` (#1).
   @sjakobi reports that this change reduced total allocations
   for building pandoc-2.12 with GHC 8.10.4 by 8.5% and reduced
   peak allocations are reduced from 3854MB to 3389MB.

## 0.3

  * Add foldlChar to signature of HasChars [API change].
  * Use foldlChar in realLength. This avoids a stack overflow
    we were getting with long strings in the previous version
    (with foldrChar).  See jgm/pandoc#6031.
  * Replace isBlank with isBreakable and improved startsWithBlank.
    Previously isBlank was used in the layout algorithm where
    what we really wanted was isBreakable.
  * Avoid unnecessary calculation in updateColumns.
  * Replace a right fold with a strict left fold.
  * Add strictness annotations in realLength and updateColumn.

## 0.2.0.1

  * Made `realLength` smarter about combining characters.
    If a string starts with a combining character, that character
    takes up a width of 1; if the combining character occurs after
    another character, it takes 0.  See jgm/pandoc#5863.
  * Improve `isBlank`, re-use in rendering code `for BreakingSpace`.
  * Fixed incorrect `Text` width in renderig blocks.

## 0.2

  * Add instances for `Doc`: `Data`, `Typeable`, `Ord`, `Read`, `Generic`.
  * Add `literal` (like `text`, but polymorphic).
  * Change some `IsString` constraints to `HasChars`.
  * Add some default definitions for methods in `HasChars`.
  * Change `offset` and `minOffset` to be more efficient (in
    simple cases they no longer render and count line lengths).
  * Add `updateColumn`.
  * Fix problem with `lblock`/`cblock`/`rblock` when `chop` is
    invoked. This caused very strange behavior in which text
    got reversed in certain circumstances.

## 0.1

  * Initial release.
