# doclayout

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
