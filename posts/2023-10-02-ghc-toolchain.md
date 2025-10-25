---
title: Introducing ghc-toolchain to GHC
description: Improving GHC's configuration logic and cross-compilation support with the brand new <code>ghc-toolchain</code> tool.
tags: ghc, haskell
---

GHC, like most high-level language compilers, depends upon a set of tools like
assemblers, linkers, and archivers for the production of machine code.
Collectively these tools are known as a *toolchain* and capture a great deal of
platform-dependent knowledge.

Traditionally, developers generate a `./configure` script using the venerable
[`autoconf`](https://www.gnu.org/software/autoconf/) tool,
then users execute this script
when they install a GHC binary distribution. The `./configure` script
determines the location of programs (such as the C compiler) and which options GHC
will need to pass them.

While this `autoconf`-centric model of toolchain configuration has served GHC well,
it has two key issues:

 * For cross-compiling to a different platform, it would be highly valuable to
   users if GHC would become a *runtime-retargetable* compiler (like `rustc` and
   `go`). That is, the user should be able to download a single GHC binary
   distribution and use it to compile not only for their local machine, but also
   any other targets that GHC supports.

 * The ten-thousand-line `sh` file that is GHC's `./configure` script has
   historically been challenging to maintain and test. Modifications to the
   `./configure` script are among the most risky changes routinely made
   to the compiler, because it is easy to introduce a bug on some specific
   toolchain configuration, and infeasible to test all possible configurations
   in CI.

To address these issues, we are introducing `ghc-toolchain`, a new way to
configure the toolchain for GHC, which will
eventually replace the existing toolchain configuration logic in the
`./configure` script.
Its main goal is to allow new compilation toolchains to be configured for GHC at
any point in time, notably after the compiler has been installed.
For example, calling `ghc-toolchain
--triple=x86_64-w64-mingw32` will configure a
compilation toolchain on the host machine capable of producing code for
an x86_64 machine running Windows using MinGW.
This is an important step towards making GHC runtime-retargetable, and since
`ghc-toolchain` is implemented in Haskell, it will be much easier to modify
and test than the `./configure` script.

In this post we explain in more detail how GHC interacts with the system toolchain and how
`ghc-toolchain` facilitates our future goal of making GHC a runtime-retargetable
compiler.

> This post was a result from my internship with the GHC team at Well-Typed this summer. It
> was originally made available in the [Well-Typed Blog](https://well-typed.com/blog/2023/10/improving-ghc-configuration-and-cross-compilation-with-ghc-toolchain/).
> I'm very grateful to Matthew, Ben, Sam, and Andreas for mentoring me
> throughout my internship  and reviewing previous iteration of this post. It
> wouldn't have been possible to complete this project without Ben Gamari's help
> -- Ben wrote the first draft of ghc-toolchain and guided me through many
> weird but interesting toolchain quirks and toolchain configuration bugs while
> developing ghc-toolchain to its full mergeable extent in GHC.

## Compiler Toolchains

<!-- more -->

GHC cannot produce executables from Haskell programs in isolation -- it requires
a correctly configured toolchain to which it can delegate some responsibilities. For
example, GHC's [native code generator
backend](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/codegens.html#native-code-generator-fasm)
is capable of generating assembly code from a Haskell program, however,
producing object code from that assembly, and linking the objects into an
executable, are all tasks done by the compilation toolchain, which is invoked by
GHC using the flags that were configured for it.

Configuring a compiler toolchain is about locating the set of tools required for
compilation, the base set of flags required to invoke each tool, and properties
of these tools. For example, this might include:

 * determining various characteristics of the platform (e.g. the word size).
 * probing to find which tools are available (the C compiler, linker,
  archiver, object merging tool, etc.),
 * identifying which flags GHC should pass to each of these tools,
 * determining whether the tools support response files to work around command-line length limits, and
 * checking for and working around bugs in the toolchain.

At the moment, when a GHC binary distribution is installed, the `./configure`
script will perform the above steps and store the results in a `settings` file.
GHC will then read this file so it can correctly invoke the toolchain programs
when compiling Haskell executables.

To cross-compile a Haskell program, a user must build GHC from source as a
cross-compiler (see the [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling)).
This requires configuring a cross-compilation
toolchain, that is, a toolchain that runs on the machine compiling the Haskell
program but that produces executables to run on a different system.  It is currently a rather involved process.

## The runtime-retargetable future of GHC

A key long-term goal of this work is to allow GHC to become *runtime-retargetable*.
This means being able to call `ghc --target=aarch64-apple-darwin` and have GHC
output code for an AArch64 machine, or call `ghc --target=javascript-ghcjs` to generate Javascript code, regardless of the platform `ghc` is being invoked on.

Crucially, this requires the configuration step to be
*repeated* at a later point, rather than only when the GHC binary distribution
is installed. Once GHC is fully runtime-retargetable, this will allow you to use
multiple different toolchains, potentially targeting different platforms, with
the same installed compiler.

 * At the simplest level, you might just have two different toolchains for your host
   platform (for example, a `gcc`-based toolchain and a `clang`-based toolchain), or
   you might just configure a toolchain which uses the new [`mold`](https://github.com/rui314/mold) linker rather than `ld.gold`.

 * In a more complex scenario, you may have a normal compiler toolchain as well as
   several different cross-compiler toolchains. For example, a toolchain which produces Javascript,
   a toolchain which produces WebAssembly, a toolchain which produces AArch64 object code and so on.

The idea is that the brand new `ghc-toolchain` will be called once to configure the toolchain
that GHC will use when compiling for a target, then `ghc --target=<triple>` can
be called as many times as needed. For example, if you have an x86 Linux machine
and wish to produce code for AArch64 devices, the workflow could look something
like:
```
# Configure the aarch64-apple-darwin target first
# (We only need to do this once!)
ghc-toolchain --triple=aarch64-apple-darwin --cc-opt="-I/some/include/dir" --cc-linker-opt="-L/some/library/dir"

# Now we can target aarch64-apple-darwin (as many times as we'd like!)
ghc --target=aarch64-apple-darwin -o MyAwesomeTool MyAwesomeTool.hs
ghc --target=aarch64-apple-darwin -o CoolProgram CoolProgram.hs
```

## Introducing `ghc-toolchain`

`ghc-toolchain` is a standalone tool for configuring a toolchain.
It receives as input a [target triplet](https://wiki.osdev.org/Target_Triplet)
(e.g.  `x86_64-deb10-linux`) and user options, discovers the configuration, and
outputs a "target description" (`.target` file) containing the configured
toolchain.

At the moment, `.target` files generated by `ghc-toolchain` can be used by GHC's
build system (Hadrian) by invoking `./configure` with
`--enable-ghc-toolchain`. Otherwise, Hadrian reads the configuration from a
`.target` file generated by `./configure` itself.

In the future, `ghc-toolchain` will be shipped in binary distributions to allow
new toolchains to be added after the compiler is installed (generating new
`.target` files). GHC will then be able to choose the `.target` file for the
particular target requested by the user.

From a developer standpoint, `ghc-toolchain` being written in Haskell makes it
easier to modify in future, especially when compared to the notoriously
difficult to write and debug `./configure` scripts.

## Migration to `ghc-toolchain`

We are migrating to `ghc-toolchain` in a staged manner, since toolchain configuration
logic is amongst the most sensitive things to change in the compiler.
We want to ensure that the configuration logic in
`ghc-toolchain` is correct and agrees with the logic in
`./configure`. Therefore, in GHC 9.10 `ghc-toolchain` will be shipped
and validated but not enabled by default.

To validate `ghc-toolchain`, GHC will generate `.target` files with both `./configure` and
`ghc-toolchain` and compare the outputs against each other, emitting a warning if they differ.
This means we will be able to catch mistakes in
`ghc-toolchain` (and in `./configure` too!) before we make `ghc-toolchain` the default
method for configuring toolchains in a subsequent release.  This mechanism has already identified
[plenty of issues to resolve](https://gitlab.haskell.org/ghc/ghc/-/issues/?label_name%5B%5D=ghc-toolchain).

<!-- There's also a `Note [ghc-toolchain overview]` in -->
<!-- `utils/ghc-toolchain/src/GHC/Toolchain.hs` -->

## Future work

Despite `ghc-toolchain` bringing us closer to a runtime-retargetable GHC, there
is still much work left to be done (see
[#11470](https://gitlab.haskell.org/ghc/ghc/-/issues/11470)).
The next step is to instruct GHC
to choose between multiple available `.target` files at runtime, instead of
reading the usual `settings` file (tracked in [#23682](https://gitlab.haskell.org/ghc/ghc/-/issues/23682)).

Beyond that, however, there are many open questions still to resolve:

* How will the runtime system, and core libraries such as `base`, be provided
  for the multiple selected targets?
* How will this fit into `ghcup`'s installation story?
* How will `cabal` handle multiple targets?

At the moment, binary distributions include the RTS/libraries already compiled
for a single target.  Instead, we are likely to need some mechanism for users to
recompile the RTS/libraries when they configure a new target, or to download
ready-built versions from upstream.

Moreover, accommodating `TemplateHaskell` under runtime retargetability is particularly
nontrivial, and needs more design work.


## Conclusion

`ghc-toolchain` is a new tool for configuring toolchains and targets. It
improves on GHC's existing `./configure`-based configuration workflow by allowing multiple targets' toolchains
to be configured at any time, and by making maintenance and future updates to
the toolchain configuration logic much easier.
However, toolchain configuration is a challenging part of the compiler, so we're
being conservative in migrating to `ghc-toolchain`, and carefully validating it
before making it the default.

Moreover, `ghc-toolchain` is an important step towards making a runtime-retargetable
GHC a reality, though there is still much work left to do.
We are grateful to all the GHC developers involved in working towards runtime-retargetability.

Well-Typed is able to work on GHC, HLS, Cabal and other core Haskell
infrastructure thanks to funding from various sponsors. If your company might be
able to contribute to this work, sponsor maintenance efforts, or fund the
implementation of other features, please
[read about how you can help](/blog/2022/11/funding-ghc-maintenance) or
[get in touch](mailto:info@well-typed.com).

