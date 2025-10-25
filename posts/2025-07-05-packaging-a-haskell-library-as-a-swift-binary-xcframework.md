---
title: Automatically Packaging a Haskell Library as a Swift Binary XCFramework
description: Announcing <code>xcframework</code> or&#58 the happy path for wiring a Haskell
              dependency to your Swift app
tags: haskell, swift
---

I've written about *Haskell x Swift* interoperability before.
[Calling Haskell from Swift](2024-04-02-calling-haskell-from-swift.html) is
about marshalling and the foreign function interface. But [Creating a macOS app
with Haskell and Swift](2023-11-10-creating-a-macos-app-with-haskell-and-swift.html) tells the
much messier tale of hijacking XCode to vodoo together the Haskell library, its
headers, and two handfuls of other magic ingredients into one buildable SwiftUI
application.

Stop! Don't click on the last link. No, it turns out that my XCode sallies
strayed very far from the yellow brick road. The IDE is confused. Recompilation
bugs abound. Complexity is through the roof juggling `.modulemap`s, `.xcconfig`
dynamic settings, and sketchy `.sh` scripts.

Let's walk the happy path.

# Announcing: xcframework

Perhaps obvious in retrospect, the *demon-less* way to add a Haskell library to
the dependencies of a Swift application is to build an independent
Swift Package wrapping the Haskell library -- something that can be done
without XCode in sight. Easy peasy:

1. Build the Haskell library using Cabal
2. Create a Swift package from the Haskell artifacts
3. Add the Swift package as a dependency to the project

And it turns out that (1) and (2) can be merged together using [Cabal
SetupHooks](https://well-typed.com/blog/2025/01/cabal-hooks/)!

Moreover, I'm happy to announce I've neatly packaged and released that build
process automation as a Haskell library called
[xcframework](https://hackage.haskell.org/package/xcframework) on Hackage.

Onwards! -- for what it does and how to use it.

## XCFrameworks

Apple introduced XCFramework bundles back in a [WWDC19
session](https://developer.apple.com/videos/play/wwdc2019/416/). An XCFramework
is a [multiplatform binary framework bundle](https://developer.apple.com/documentation/xcode/creating-a-multi-platform-binary-framework-bundle).

For our purposes, that means we can create a Swift Package just from a binary
linkable artifact and a couple of header files. Then, any Swift project can
depend on this binary Swift package and call the functions exposed to the
headers and make sure the bundled library will be linked in with the final
executable. Specifically, the `xcframework` Haskell library, for a given
Haskell library, bundles:

- The foreign shared library (`.dylib`) resulting from building with GHC/Cabal
- The foreign export headers generated from the `foreign export <haskell_function>` declarations
- The RTS headers
    - which are needed to initialize the RTS from Swift
    - and because they are `#include`d by the `foreign export` headers
- A `.modulemap` exporting the foreign exported functions and `HsFFI.h`
    - The module map basically turns the headers into a Swift module that can
      be transparently `import`ed from other Swift modules.

And any Swift library or application can transparently depend on this
`.xcframework` and use the foreign exported Haskell functions without further
ado.

## How to install `xcframework`

In your cabal file, change the `build-type` to `Hooks` (and set `cabal-version:
3.14` if not set already):

```diff
- build-type:     Simple
+ build-type:     Hooks
```

And add a `setup-depends` stanza with a dependency on `xcframework`:

```txt
custom-setup
  setup-depends:
    base        >= 4.18 && < 5,
    xcframework >= 0.1
```

Finally, create a file called `SetupHooks.hs` in the root of your Cabal package
with the following contents, substituting the `_build/MyHaskellLib.xcframework` string for the
filepath to where the `.xcframework` should be written:

```haskell
module SetupHooks ( setupHooks ) where

import Distribution.XCFramework.SetupHooks

setupHooks :: SetupHooks
setupHooks = xcframeworkHooks "_build/MyHaskellLib.xcframework"
```

Now, whenever you run `cabal build`, the built libraries will also be bundled into an `.xcframework`.

## How to use the XCFramework in XCode

In XCode:

1. Navigate to the target settings of your project.
2. Find under "General" the "Frameworks, Libraries, and Embedded Content" (or similar) section.
3. Click the add button and add the `.xcframework` framework outputted at the specified path by Cabal

Now, in the entry Swift module, import the RTS and init/exit the RTS. For
instance, in a sample SwiftUI app:

```diff
  import SwiftUI
+ import Haskell.Foreign.Rts
  
  @main
  struct MyExample: App {
+ 
+     init() {
+         hs_init(nil, nil)
+
+         NotificationCenter.default
+           .addObserver(forName: NSApplication.willTerminateNotification,
+                        object: nil, queue: .main) { _ in
+           hs_exit()
+         }
+     }
+ 
      var body: some Scene {
          WindowGroup {
              ContentView()
          }
      }
  }
```

Finally, in any Swift module, do `import Haskell.Foreign.Exports`. For now, the
name `Haskell.Foreign.Exports` is fixed and exports all foreign-exported
functions, but it could be improved in the future (perhaps it's a good task to
contribute a patch for!)

For example, if your Haskell module looked like:

```haskell
module MyLib (doSomething) where

fib :: Integral b => Int -> b
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

doSomething :: IO Int
doSomething = do
  putStrLn "doing some thing"
  return $ fib 42

foreign export ccall doSomething :: IO Int
```

In your Swift module you can now

```swift
import Haskell.Foreign.Exports

  ...
  let x = doSomething()
  ...
```

## Building simple Swift package

The `.xcframework` can also be easily used in a standalone swift package built
with `swift build`.

In your `Package.swift`, add `MyHaskellLib.xcframework` as a binary target and
make it a dependency of your main target. For instance, a simple library would
look like:

```swift
// swift-tools-version: 6.1
import PackageDescription

let package = Package(
    name: "MySwiftLib",
    platforms: [
        .macOS(.v15)
    ],
    products: [
        .library(name: "MySwiftLib", targets: ["MySwiftLib"])
    ],
    targets: [
        .target(name: "MySwiftLib", dependencies: ["MyHaskellLib"], path: "Swift"),
        .binaryTarget(
            name: "MyHaskellLib",
            path: "haskell/_build/MyHaskellLib.xcframework"
        )
    ]
)
```

Now you can use the `Haskell.Foreign.Exports` import in any module in the
package as explained above, for instance in `Swift/MySwiftLib.hs`:

```swift
import Foundation
import Haskell.Foreign.Exports

public struct Fib {
  var val: Int64
}

public func mkFib() -> Fib {
    let x = doSomething()
    return Fib(val: x)
}
```

Build the Swift package using `swift build` in the project root.

## Must use Cabal Foreign Library stanza

Unfortunately, while I don't figure out how to link the right amount of things
into the `.xcframework` after building a normal `library` component in Cabal,
the `foreign export`s must be exported from a `foreign-library` Cabal stanza:

```txt
foreign-library myexample
    type: native-shared
    options: standalone
    other-modules:    MyLib
    build-depends:    base ^>=4.20.0.0
    hs-source-dirs:   src
    default-language: GHC2021
```

To clarify the instructions, I put together [a small demo
project](https://github.com/alt-romes/hs-xcframework-simple-demo/) with a
working setup -- if you want to try it out. Remember to build the Cabal library first!

## Conclusion

Building the Haskell library as an independent Swift Package is a much more
robust way of adding a Haskell dependency to a Swift application.

The `xcframework` Haskell library makes it easy to create XCFrameworks from
Haskell packages by leveraging the `SetupHooks` [very nicely designed
API](https://hackage-content.haskell.org/package/Cabal-hooks/docs/Distribution-Simple-SetupHooks.html).

While this work further lowers the bar for integrating Haskell and Swift,
marshaling and sharing high-level datatypes remains challenging. [Calling
Haskell from Swift](2024-04-02-calling-haskell-from-swift.html) explored the
basics of using more interesting types across the FFI, but I'm also working on
a more automated approach using TH and GHC plugins.

Finally, I'm looking forward to [bug reports](https://github.com/alt-romes/haskell-swift) if you try it out.
