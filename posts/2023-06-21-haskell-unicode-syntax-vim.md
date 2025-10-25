---

title: Writing prettier Haskell with Unicode Syntax and Vim

description: A short write-up on combining <em>digraphs</em>, a feature built-in
                to vim, and Haskell's <code>UnicodeSyntax</code> extension, to easily write
                beautiful Haskell programs with unicode symbols.

tags: haskell, vim

---

# Haskell's Unicode Syntax Extension

Haskell (well, GHC Haskell) features an extension called `UnicodeSyntax`. When
enabled, this extension allows the use of certain unicode symbols in place of
their corresponding keywords. A great example is the `forall` keyword being
equivalent to the unicode symbol `∀`, the two of which can be used
interchangebly when `UnicodeSyntax` is enabled.

Furthermore, with Haskell being a unicode-friendly language, one can define
common Haskell functions, operators or type variables using unicode symbols --
which doesn't even require `UnicodeSyntax` to be enabled. For example, one can
define the predicate `∈` on lists as an alias for `elem` as follows:
```haskell
-- 5 ∈ [1,3,5] == True
(∈) :: ∀ α. Eq α => α -> [α] -> Bool
(∈) = elem
```
In practice, I use just a handful of unicode symbols both as keywords and as
identifiers, but a mostly comprehensive list of the keywords that have unicode
alternatives is presented in the GHC user's guide [`UnicodeSyntax` extension
page](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/unicode_syntax.html).
Specifically, in most of my programs you can be sure to find the following:

- `∀` instead of `forall`, which is faster to input than the whole word.
- A lot of unicode type variables, `α`, `β`, `τ`, `σ`, `δ`, `κ`, `ρ` -- they are
    really easy to type too.
- `⊸` instead of `%1 ->`, to use the so-called "lollipop" notation for linear
    functions.
<!-- , which is read "lollipop" rather than "what the h★ll" -->

In my opinion, those are low-hanging niceties (with vim) that make the program
look better overall, but there are others that I haven't yet reached for
which you may still find good/useful. For example, there's a library in
hackage, [containers-unicode-symbols](https://hackage.haskell.org/package/containers-unicode-symbols),
which exposes multiple unicode variants of functions on containers
(`Map`s,`Set`s,...) such as `∈`,`∉`,`∅`,`∪`,`∩`,`⊆`,`⊈`.

Finally, I usually add `default-extensions: UnicodeSyntax` to my cabal
file to make the extension available by default on all modules. However, you can
also enable it on a per module basis as usual with `{-# LANGUAGE UnicodeSyntax
#-}` at the top of the module.

# Digraphs in Vim

To experiment with `UnicodeSyntax`, or if you're already convinced that using
unicode symbols makes the programs nicer to look at, all that's left is to be
able to input these symbols easily.
Vim has a built-in feature called *digraphs* that makes inputting unicode
symbols a joy. The feature is called *digraphs* because it maps combinations of
exactly two letters to a unicode symbol (see also `:help digraph`).

To input a digraph, in <span class="smallcaps">insert mode</span>, press
<kbd>Ctrl</kbd>+<kbd>k</kbd> followed by the two letters which define the digraph.
Here are a few useful, *built-in*, combinations:

- <kbd>Ctrl-k</kbd>+<kbd>FA</kbd> inputs `∀`.
- <kbd>Ctrl-k</kbd>+
    - <kbd>a*</kbd> inputs `α`.
    - <kbd>b*</kbd> inputs `β`.
    - <kbd>t*</kbd> inputs `τ`.
    - In general, <kbd>Ctrl-k</kbd>+<kbd>letter</kbd>+<kbd>*</kbd> inputs the greek letter variant
        of that letter
- <kbd>Ctrl-k</kbd>+<kbd>(-</kbd> inputs `∈`.
- <kbd>Ctrl-k</kbd>+<kbd>::</kbd> inputs `∷`.
- <kbd>Ctrl-k</kbd>+<kbd>=></kbd> inputs `⇒`.
- <kbd>Ctrl-k</kbd>+<kbd>-></kbd> inputs `→`.
- <kbd>Ctrl-k</kbd>+<kbd>TE</kbd> inputs `∃`.

<!-- ## Custom Digraphs -->

Besides the built-in ones, it's *very* useful to define your own digraphs. Both
for customization/personalization and ergonomics, but also to introduce digraphs
which simply do not exist by default.

To create a digraph, use the `digraph` VimScript keyword with the two characters
that input it, and the decimal numeric representation of the Unicode character
it is mapped to. In the following `.vimrc` snippet, I defined the digraph `ll`
with `8888` (the unicode decimal representation of ⊸), which effectively maps
<kbd>Ctrl-k</kbd>+<kbd>ll</kbd> to `⊸`.
```haskell
digraph ll 8888 " ⊸
```

# Conclusion

Concluding, vim makes it really easy, through *digraphs*, to input unicode
symbols which are understood by Haskell, and even more so with its
`UnicodeSyntax` extension. Combining these features we can easily write more beautiful
Haskell programs. I'd argue it's as fast to write
```haskell
lid :: ∀ α. α ⊸ α
```
as it is to write
```haskell
lid :: forall a. a %1 -> a
```
while the former is arguably more aesthetically pleasing.

