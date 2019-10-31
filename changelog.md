# doclayout

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
