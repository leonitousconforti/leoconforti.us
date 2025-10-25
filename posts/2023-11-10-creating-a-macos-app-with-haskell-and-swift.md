---

title: Creating a macOS app with Haskell and Swift

description: First part of an in-depth guide into developing a native macOS application using
             Haskell with Swift and SwiftUI. This part covers the set-up required to call Haskell
             functions from Swift in an XCode project using SwiftUI.


tags: haskell, swift, macos

---

This is the first part of an in-depth guide into developing a native
applications for Apple platforms (macOS, iOS, etc.) using Haskell with Swift and
SwiftUI. This is the first in a series of blog posts -- covering the set-up
required to call Haskell functions from Swift in an XCode project using SwiftUI.
In future installements of the series, I intend to at least discuss calling
functions with idiomatic Haskell types with Swift ones (both with and without
marshaling), SwiftUI observation, and iOS development which requires GHC to
produce code for the iOS compilation target.

At the time of writing I'm using XCode 15, Cabal 3.10, and GHC 9.8. There will
be some features I use that are only available in these recent versions,
however, the general idea of interoperability between Haskell and Swift stands
on its own regardless -- the now 7 year
old [swift-haskell-tutorial](https://github.com/nanotech/swift-haskell-tutorial/tree/master) is still similarly relevant and greatly informed
my approach, despite the end result being considerably different.

The end goal is to create a multi-(apple)-platform application whose UI is
programmed in Swift using SwiftUI while the data and logic of the application is
implemented in Haskell which is called from Swift.

The series of blog posts is further accompanied by a github repository where
each commit matches a step of this tutorial. If in doubt regarding any step,
simply checking the matching commit for absolute confidence you are
understanding the practical step correctly. [Visit this link to the haskell-x-swift-project-steps repository](https://github.com/alt-romes/haskell-x-swift-project-steps)!
Furthermore, I'm writing a build tool that will facilitate setting up and
building a project like this without having to go through all the manual steps: [haskell-swift](https://github.com/alt-romes/haskell-swift).

This write-up has been cross-posted to [Well-Typed's Blog](https://well-typed.com/blog/).

# Hello, Swift, it's Haskell!

In this part we are only concerned with getting our `Hello, World!` going.

1. We'll setup a Haskell (foreign) library exporting a function `hs_factorial` that
    returns the factorial of integer, using the C FFI
2. Setup a SwiftUI app that calls `hs_factorial`
3. Compile the Haskell code into a shared library
4. Create a Swift module `HaskellFramework` to export the Haskell functions
   (imported from the stub C header files), and setup linking against the
   Haskell shared library.
5. Import `HaskellFramework` into the SwiftUI app to be able to successfully
   call `hs_factorial` and display the result on the screen of the running
   application.

The following diagram (rendered by [Diagon](https://arthursonzogni.com/Diagon/#GraphDAG)) describes how the Swift executable and Haskell libraries
are going to be connected from a not-too-far-away perspective. It might be
useful to consult this diagram once in a while throughout the post!
<!-- Source code of graph @ https://arthursonzogni.com/Diagon/#GraphDAG

Haskell library -> Haskell foreign library
cbits -> Haskell foreign library
cbits -> Headers (cbits)
Haskell foreign library -> Headers (stubs)
Haskell foreign library -> Shared dynamic library
Headers (stubs) -> Clang modules
Headers (cbits) -> Clang modules

gen-dynamic-settings.sh -> DynamicBuildSettings.xcconfig
RTS headers -> DynamicBuildSettings.xcconfig
Shared dynamic library -> DynamicBuildSettings.xcconfig
DynamicBuildSettings.xcconfig -> BuildSettings.xcconfig

Clang modules -> SwiftUI App
Shared dynamic library -> SwiftUI App
BuildSettings.xcconfig -> SwiftUI App
-->
```txt
┌───────────────┐┌───────────┐┌───────────────────────┐┌───────────┐
│Haskell library││cbits      ││gen-dynamic-settings.sh││RTS headers│
└┬──────────────┘└┬─────────┬┘└─────────────┬─────────┘└┬──────────┘
┌▽────────────────▽───────┐┌▽──────────────┐│           │
│Haskell foreign library  ││Headers (cbits)││           │
└┬───────────────────────┬┘└─────────────┬─┘│           │
┌▽─────────────────────┐┌▽──────────────┐│  │           │
│Shared dynamic library││Headers (stubs)││  │           │
└┬────────────────────┬┘└┬──────────────┘│  │           │
 │            ┌───────│──│───────────────┘  │           │
 │            │┌──────│──┘                  │ ┌─────────┘
 │┌───────────▽▽┐┌────▽─────────────────────▽─▽┐
 ││Clang modules││DynamicBuildSettings.xcconfig│
 │└┬────────────┘└┬────────────────────────────┘
 │ │┌─────────────▽────────┐
 │ ││BuildSettings.xcconfig│
 │ │└┬─────────────────────┘
┌▽─▽─▽──────┐
│SwiftUI App│
└───────────┘
```

## Setting up the SwiftUI app

Let's set-up a simple XCode project using SwiftUI for the main interface.  Fire
up XCode and create a macOS Application, named `SwiftHaskell`, using SwiftUI,
excluding tests. Choose a Personal Team rather than None - you might have to
create a (free of charge) one.

In the newly-created project there should exist two files: `SwiftHaskellApp.swift` and `ContentView.swift`.
We can change right away `ContentView.swift` to display the result of calling
`hs_factorial(5)`, even though `hs_factorial` is not yet in scope:
```swift
import SwiftUI

struct ContentView: View {
    var body: some View {
        VStack {
            Text("Hello, Haskell: \(hs_factorial(5))!")
        }
        .padding()
    }
}
```

Before proceeding to the Haskell side, create a `New File > Configuration
Settings File` (also known as a `.xcconfig` file) named
`BuildSettings.xcconfig`. We'll use this file to write all our build settings
textually instead of using XCode's build settings navigator.

To use our `.xcconfig` file for the project settings, under `Info > Configurations`
in the project tab, select the `BuildSettings` file.
For the configuration to show up in XCode, the `.xcconfig` must be in the tree
navigator (which happens by default if you created the module within XCode).
You can read more, or see exactly how to set an `.xcconfig` file as the
configuration, in this [write-up on `xcconfig`](https://nshipster.com/xcconfig/)
by NSHipster.

Even though we are setting the `.xcconfig` file manually (and also e.g.
initializing the XCode project), it is possible to resort to an exclusively
programatic approach using so-called XCode project generators such as
[XCodeGen](https://github.com/yonaskolb/XcodeGen)
and [Tuist](https://tuist.io/).

## Setting up a Haskell foreign library

Create a folder `haskell-framework` within the XCode project, `cd` into it, and
follow from there.

We're jumping straight into a full-fledged Haskell projected managed with cabal,
where we define a shared library using the `foreign-library` stanza.

Start with a normal cabal file with a `library` stanza that exposes `MyLib` (by
running `cabal init` within `haskell-framework`), and add the function `hs_factorial` to `MyLib` that
operates on `CInt`s:
```haskell
module MyLib where
import Foreign.C

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]
```
The organization of the code here isn't terribly important. Perhaps in a
real project you could want to, for instance, only use C types like `CInt`
in the foreign library bits.

In the cabal file, add a `foreign-library` stanza with
```haskell
foreign-library haskell-foreign-framework
    type: native-shared

    -- This should work on Mac, despite being undefined behaviour
    -- See https://www.hobson.space/posts/haskell-foreign-library/ (great read)
    options: standalone

    -- We copy the C stub headers to a folder in the root.
    -- If you have foreign-export declarations in the library
    -- be sure to add this flag there too (so all stubs get added
    -- to the `haskell-framework-include` folder)
    ghc-options: -stubdir=haskell-framework-include

    other-modules: MyForeignLib
    build-depends: base, haskell-framework
    hs-source-dirs: flib
```
Unfortunately, `options: standalone` is only officially supported (and
required) by Windows, even though it is exactly what we need. However,
unofficially, a macOS distribution should be able to safely use this option
-- for more information see this [write-up on foreign libraries explaining why this option is
undefined for macOS](https://www.hobson.space/posts/haskell-foreign-library/).
<!---->
In the future, this might work out of the box without being undefined
behaviour, or the behaviour on macOS may have changed s.t. this no longer
works... but let's hope for the former.
<!---->
Additionally, we pass `-stubdir` for GHC to output the C stub header files to a
directory `haskell-framework-include`. Do add this automatically generated
directory to `.gitignore`.

Create the file `flib/MyForeignLib.hs` that declares a `foreign export` of
`hs_factorial` imported from `MyLib` and `foreign export`s it:
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
module MyForeignLib where
import Foreign.C
import MyLib (hs_factorial)
foreign export ccall hs_factorial :: CInt -> CInt
```
It doesn't seem that re-exporting the function from `MyLib` when it is foreign
exported from there is enough for it to be included in the shared library (might
be a bug), we do need the `foreign export` here rather than in `MyLib`.

Running `cabal build` should now generate a `haskell-framework-include` folder with a
`MyForeignLib_stub.h`, and a `libhaskell-foreign-framework.dylib` shared library
somewhere under `dist-newstyle` (you can `find . -name
libhaskell-foreign-framework.dylib` to find it)

We'll test C program against this library to check whether it works as expected.
Create `scripts/test-haskell-foreign-lib.sh` with a script that compiles a
main function in C which calls `hs_factorial`. A few notes:

- We need to pass the path to the built shared library (`$HS_FLIB_PATH`)
    to the compiler.
- We need to pass the path to the headers (`$HS_HEADERS_PATH`).
- We hardcode into the executable the path to the shared library as an `rpath`
    search path (just for testing purposes).
    When building the macOS app, XCode will add `@executable_path/../Frameworks`
    to the `rpath` search path, so we can simply copy the shared library the
    Apple-blessed location (`Frameworks`).
- We need to call `hs_init` and `hs_exit` to init the runtime system
    (see the [relevant GHC user guide section](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc)).
- We need to compile the C library using `ghc`, as it will automatically
    include and link the rts headers and library. To use a C compiler
    we'd also need to find the rts headers and library of our Haskell
    installation.

```bash
#!/usr/bin/env bash

set -e

if ! test -f "haskell-framework.cabal"; then
    echo "Run this script from the root of your project!"
    exit 1
fi

HS_FLIB_PATH=$(dirname $(find . -name libhaskell-foreign-framework.dylib))
HS_HEADERS_PATH=haskell-framework-include

echo "
#include <stdio.h>
#include <MyForeignLib_stub.h>
#include <HsFFI.h>
int main(void) {
    hs_init(NULL, NULL);
    printf(\"%d\n\", hs_factorial(5));
    hs_exit();
    return 0;
}
" > conftestmain.c

# We use `ghc` instead of `gcc` because otherwise we also need to provide the
# include and lib path of the runtime system (Rts)
ghc -no-hs-main -o conftest conftestmain.c \
    -lhaskell-foreign-framework \
    -I"$HS_HEADERS_PATH" \
    -L"$HS_FLIB_PATH" \
    -optl-Wl,-rpath,"$HS_FLIB_PATH"

RESULT=$(./conftest)

if [ 120 -eq $RESULT ]; then
    echo "Foreign library successfully called!"
else
    echo "Bad bad foreign library!"
    exit 1
fi

rm -f conftest*
```
You should get `Foreign library successfully called!` when this script is run.

## Linking the Haskell library with the executable

Our recipe for invoking a foreign exported Haskell function in Swift:

1. Create a Swift *module* exporting Haskell functions through a module map
   pointing to the headers exporting the Haskell functions.
2. Extend the *module search path* with the location of your new module map.
3. Import that module as a module in the SwiftUI code, and use the desired function.
4. At link time, the shared library with the symbols used by the program must be
   linked against, and must be found in the *run-path* which can be done by copying
   the shared library into the app bundled `Frameworks` folder.

We create a module map file listing all the headers exporting Haskell functions
to define Swift modules where Haskell functions will live, using [Clang's module
system](https://clang.llvm.org/docs/Modules.html). A module map looks something like
```haskell
module HaskellFramework {
    header "haskell-framework/haskell-framework-include/MyForeignLib.h"
    export *
}
```
and can be imported into Swift code with `import HaskellFramework`, as long as
the module map is available as `module.modulemap` in the *import search path*.
As one might expect, importing this module brings into scope all names exported
from the listed header(s).

Specifically, we will use the [inferred submodules](https://clang.llvm.org/docs/Modules.html#submodule-declaration)
feature of modules to create our module map.  With inferred submodules, we can
simply define an **umbrella** directory with headers and get a submodule for each
header in that directory (arbitrarily nested, where a header `A/B/C.h` becomes a
submodule named `MainModule.A.B.C`)

In the root of the XCode project, write a `module.modulemap` file:
```haskell
module HaskellFramework {
    umbrella "haskell-framework/haskell-framework-include"
    
    explicit module * {
        export *
    }
    
}
```
The `umbrella` keyword specifies the directory where to find the header files
for our submodules, and the `explicit module *` lines are the *inferred
submodule* part, as each header will result in a declaration roughly like
`explicit module HeaderName { header "umbrella/HeaderName.h" ... }`.
In effect, our module map above will expand to:
```haskell
module HaskellFramework {
    explicit module MyForeignLib {
        header "haskell-framework/haskell-framework-include/MyForeignLib.h"
        export *
    }
}
```
Again, to be clear, this is what our original `module.modulemap` using the
`umbrella` keyword currently expands to, **not the file we wrote**.

Having written our `module.modulemap`, we need to extend the compiler's *import
search path* to find this module map. As we've already set-up our `xcconfig`-based
configuration, this amounts to writing into `BuildSettings.xcconfig`:
```bash
SWIFT_INCLUDE_PATHS=$(PROJECT_DIR)
```
This is equivalent to changing the `Swift Compiler - Search Paths > Import
Paths` build setting in XCode (in fact, by inspecting that setting on the
rightmost inspector panel, you will find the corresponding `xcconfig` name is indeed
`SWIFT_INCLUDE_PATHS` -- this is also all explained in the [`xcconfig`
article](https://nshipster.com/xcconfig/)).

Returning to `ContentView.swift`, where `hs_factorial` is being called, you
should be able to add at the top of the file, and have XCode successfully recognize:
```swift
import HaskellFramework.MyForeignLib_stub
```
Even though the import is recognized, it will not compile successfully. The
reason is our stub header (`MyForeignLib_stub.h`) includes `<HsFFI.h>` which
cannot be found by XCode. We need to extend our *Header Search Path* with the
path to the RTS headers.

Currently, our `BuildSettings.xcconfig` can only contain statically known
information. Fortunately, we can `#include` other `xcconfig` files (that may
have been generated dynamically) in our `BuildSettings.xcconfig` (as described
by the [`xcconfig` write-up](https://nshipster.com/xcconfig/)).
We do this by adding the following include directive in the `BuildSettings.xcconfig` file:
```c
#include "DynamicBuildSettings.xcconfig"
```
We will generate `DynamicBuildSettings.xcconfig` with a script
`haskell-framework/scripts/gen-dynamic-settings.sh` that calls the
```sh
ghc-pkg field rts include-dirs --simple-output
```
to figure out the rts include path of the existing GHC installation.

We extend `HEADER_SEARCH_PATHS`, the `xcconfig` variable listing the paths where
XCode will search for headers when building, with the path to the RTS header
files:
```bash
#!/usr/bin/env bash

set -e
if ! test -f "haskell-framework/haskell-framework.cabal"; then
    echo "Run this script from the root of your XCode project!"
    exit 1
fi

echo "
HEADER_SEARCH_PATHS=\$(inherit) $(ghc-pkg field rts include-dirs --simple-output | tr ' ' '\n' | tail -n1)
" > DynamicBuildSettings.xcconfig

echo "Created DynamicBuildSettings.xcconfig!"
```
Do add `DynamicBuildSettings.xcconfig` to `.gitignore`.
The literal string `$(inherit)` is `xcconfig` syntax for inheriting the options
set before applying this configuration. Furthermore,
asking for the `include-dirs` of `rts` outputs two directories:
```txt
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/ffi
/Users/romes/.ghcup/ghc/9.8.1/lib/ghc-9.8.1/lib/../lib/aarch64-osx-ghc-9.8.1/rts-1.0.2/include
```
However, the `ffi` header is already included in a module by default in XCode
applications, so we need to cut it out of the search paths to avoid a
`Redefinition of module 'FFI'` error (`tr` combined with `tail -n1` select just
the path to the RTS headers).

The function is now found by XCode as the module it is defined in compiles
successfully and brings the function into scope.
However, building the program will fail with a *link time* error:
even though we instructed the compiler to find the definitions of the Haskell
functions we want to use and the module they are exported from, we have not
linked against the library where the actual symbols are defined.

The Haskell foreign library created in a previous section compiles to a
*shared dynamic library*. To link against it when building our Swift application
we need to pass `-lhaskell-foreign-framework` to the compilation toolchain and
instruct it on where to find this library. The first step can be done in two
compatible (as in both can co-exist) ways:

- Add a `link "haskell-foreign-framework"` declaration to the module map (explained [here](https://clang.llvm.org/docs/Modules.html#link-declaration))
    - There is a note about this feature not yet being widely supported in the
        reference page, however, it is works to link the library in my machine
        with XCode 15.
- Add the `-lhaskell-foreign-framework` flag to the `OTHER_LDFLAGS` build
    setting in `BuildSettings.xcconfig`. You can do this even if you've also
    used the link directive.

After adding the `link` declaration, your `module.modulemap` should contain:
```haskell
module HaskellFramework {
    umbrella "haskell-framework/haskell-framework-include"
    
    explicit module * {
        export *
    }

    link "haskell-foreign-framework"
}
```

Secondly, we need to add the shared library path to the *library search path*
and make it available at runtime by copying it to the `Frameworks` folder that
is bundled with the application.
By copying the library to this folder we ensure it can be found when dynamically
loaded at runtime: the library install name is relative to `@rpath`, i.e. it is
a [*run-path dependent library*](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/RunpathDependentLibraries.html),
and the run-path dependencies of XCode built executables are searched for in the
Frameworks folder, relatively to the executable path (`@executable_path/../Frameworks`).

> A run-path dependent library is a dependent library whose complete install
> name is not known when the library is created (see How Dynamic Libraries Are
> Used). Instead, the library specifies that the dynamic loader must resolve the
> library’s install name when it loads the executable that depends on the
> library.
>
> To use run-path dependent libraries, an executable provides a list of run-path
> search paths, which the dynamic loader traverses at load time to find the
> libraries.

In practice, we achieve this by extending the `LIBRARY_SEARCH_PATHS` setting
dynamically and add a "Copy" Build Phase which copies the shared library to the
listed Frameworks folder. At this time, I do not know how to do this Copy
outside of XCode -- do shoot me a text if you know how. It is also unfortunate
that we have to hardcode the path to the dynamic library there, instead of
computing it at build time.

Find the path to the foreign library by running, in the `haskell-framework` directory:
```txt
cabal list-bin haskell-foreign-framework
```
Then, under the Build Phases tab of the project settings, add (by clicking in
the little plus sign) a `New Copy Files Phase`. Then, clicking in the plus sign of
the new listing of files to copy, add the haskell-foreign-framework
`.dylib` (the shared library) that lives at the path found by running the above
command by clicking on "Add Other".

To the `haskell-framework/scripts/gen-dynamic-settings.sh`, add the following
lines before echoing to the file
```bash
pushd . > /dev/null
cd haskell-framework
FLIB_PATH=$(cabal list-bin haskell-foreign-framework)
popd > /dev/null
```
and to what is written to `DynamicBuildSettings.xcconfig` add the following line
```bash
LIBRARY_SEARCH_PATHS=\$(inherit) $(dirname $FLIB_PATH)
```

At this point, after regenerating the dynamic build settings, you should be able
to link the application successfully, and run it.

## The RTS must be initialized

Surprise! Running the application will fail at runtime, when `hs_factorial` is
called. To call Haskell functions from an executable written in another language,
one must first initialize the GHC runtime system, and terminate it when
appropriate. We need to call the functions `hs_init` and `hs_end`, exposed in
`HsFFI.h`. We will write two wrapper functions in our foreign library to invoke
instead, as suggested in the [FFI chapter of the GHC user guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc).

We create a `cbits` folder in the `haskell-framework` Haskell project to put our
C files and headers, and add them to the `foreign-library` stanza of the cabal
file:
```haskell
include-dirs: cbits
c-sources: cbits/MyForeignLibRts.c
install-includes: MyForeignLibRts.h
```
You can see what these options do in [this cabal user guide section](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-includes).
We create `cbits/MyForeignLibRts.c` wrapping the calls to `hs_init` and
`hs_end` as described in the FFI chapter linked above:
```c
#include <stdlib.h>
#include <stdio.h>
#include <HsFFI.h>

HsBool flib_init() {

    printf("Initialising flib\n");

    // Initialise Haskell runtime
    hs_init(NULL, NULL);

    // Do other library initialisations here

    return HS_BOOL_TRUE;
}

void flib_end() {
    printf("Terminating flib\n");
    hs_exit();
}
```
It might seem that you could `foreign import` these functions into the Haskell
library and re-export them with `foreign export`, however, if they are exported
from Haskell, they themselves require the RTS to be initialised, effectively
defeating the purpose of being functions that initialise the RTS. Therefore, we
write a header file that we ship with the library for it to be included by the
Swift project. The file `cbits/MyForeignLibRts.h` contains:
```c
#include <HsFFI.h>

HsBool flib_init();
void flib_end();
```

Back to the Swift side, we need to augment our module map with a module mapping
to the RTS initialisation wrapper header. We add a second submodule declaration:
```haskell
explicit module RTSManage {
   header "haskell-framework/cbits/MyForeignLibRts.h"
}
```
The `cbits/MyForeignLibRts.c` symbols will be included in the shared dynamic
library.

You can re-buid the haskell library and re-generate the dynamic settings with a
script `./build-haskell` in the root of the XCode project:
```sh
#!/usr/bin/env bash

set -e

if ! test -d "SwiftHaskell.xcodeproj"; then
    echo "Run this from the SwiftHaskell XCode project root!"
    exit 1
fi

pushd . >/dev/null
cd haskell-framework/
cabal build all --allow-newer
./scripts/test-haskell-foreign-lib.sh
popd >/dev/null
./haskell-framework/scripts/gen-dynamic-settings.sh

echo "Done."


```

Finally, in `SwiftHaskellApp.swift`, we extend the `@main` `App` by overriding
the `init()` function: calling `flib_init()` to initialise the runtime system
and setting up an observer to call `flib_end()` to end the runtime system when
the application terminates. We need only import `HaskellFramework.RTSManage` to
bring these functions into scope:
```swift
@main
struct SwiftHaskellApp: App {

    init() {
        flib_init()

        NotificationCenter.default.addObserver(forName: NSApplication.willTerminateNotification, object: nil, queue: .main) { _ in
            // terminating
            flib_end()
        }
    }

    ...
}
```
Running your application should work and proudly print `120` on the screen.

# Remarks

We've come to the end of the first installment in this blogpost series.
Next up is communicating more interesting data types (both with and without
marshalling), making things more ergonomic to use, SwiftUI observation, iOS
compilation, and perhaps developing a simple model app.

The
[haskell-x-swift-project-steps](https://github.com/alt-romes/haskell-x-swift-project-steps)
git repository has a commit matching each of the steps of this guide, so if
anything is unclear you can just let the code speak by itself in checking the
commits.

This project, blog post, and research regarding Swift interoperability with
Haskell is being partially sponsored by [Well-Typed](https://well-typed.com/),
and is otherwise carried out in my own free time. If you'd also like to sponsor my
work on Swift x Haskell interoperability with the goal of developing native
macOS/iOS/etc applications, visit [my GitHub sponsors
page](https://github.com/sponsors/alt-romes).

## Further Reading

- [swift-haskell-tutorial by nanotech](https://github.com/nanotech/swift-haskell-tutorial/tree/master)
- [Haskell foreign library and options: standalone](https://www.hobson.space/posts/haskell-foreign-library/)
- [Using the FFI with GHC](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/ffi.html#using-the-ffi-with-ghc)
- [xcconfig by NSHipster](https://nshipster.com/xcconfig/)
- [Clang module](https://clang.llvm.org/docs/Modules.html)
- [Run-path dependent libraries](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/RunpathDependentLibraries.html)
- [Cabal user guide](https://cabal.readthedocs.io/en/stable/)

