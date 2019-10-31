# doclayout

[![CI
tests](https://github.com/jgm/doclayout/workflows/CI%20tests/badge.svg)](https://github.com/jgm/doclayout/actions)

This is a prettyprinting library designed for laying out
plain-text documents.  It originated in the pandoc module
Text.Pandoc.Pretty, and its development has been guided by
pandoc's needs in rendering wrapped textual documents.

In supports wrapping of text on breaking spaces, indentation
and other line prefixes, blank lines, and tabular content.

Example:

``` haskell
Text.DocLayout> mydoc = hang 2 "- " (text "foo" <+> text "bar")
Text.DocLayout> putStrLn $ render (Just 20) mydoc
- foo bar
Text.DocLayout> putStrLn $ render (Just 10) (prefixed "> " (mydoc $+$ mydoc))
> - foo
>   bar
>
> - foo
>   bar
```

The `Doc` type may be parameterized to either `String` or
(strict or lazy) `Text`, depending on the desired render target.

