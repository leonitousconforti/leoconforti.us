---

title: Calling Haskell from Swift

description: Crossing the language boundary between Haskell and Swift.
             This is the second part of an in-depth guide into developing native
             applications using Haskell with Swift.

tags: haskell, swift

---

This is the second installment of the in-depth series of blog-posts on
developing native macOS and iOS applications using both Haskell and
Swift/SwiftUI. This post covers how to call (non-trivial) Haskell functions from
Swift by using a foreign function calling-convention strategy similar to that
described by [Calling Purgatory from Heaven: Binding to Rust in
Haskell](https://well-typed.com/blog/2023/03/purgatory/) that requires argument
and result marshaling.

You may find the other blog posts in this series interesting:

1. [Creating a macOS app with Haskell and Swift](2023-11-10-creating-a-macos-app-with-haskell-and-swift.html)

The series of blog posts is further accompanied by a [github
repository](https://github.com/alt-romes/haskell-x-swift-project-steps) where
each commit matches a step of this tutorial. If in doubt regarding any step,
check the matching commit to make it clearer.

This write-up has been cross-posted to [Well-Typed's Blog](https://well-typed.com/blog/).


# Introduction

We'll pick up from where the last post ended -- we have set up an XCode project
that includes our headers generated from Haskell modules with `foreign export`s
and linking against the foreign library declared in the cabal file. We have
already been able to call a very simple Haskell function on integers from Swift
via Haskell's C foreign export feature and Swift's C interoperability.

This part concerns itself with calling idiomatic Haskell functions, which
typically involve user-defined datatypes as inputs and outputs, from Swift.
Moreover, these functions should be made available to Swift transparently, such
that Swift calls them as it does other idiomatic functions, with user defined
structs and classes.

For the running example, the following not-very-interesting function will
suffice to showcase the method we will use to expose this function from Haskell
to Swift, which easily scales to other complex data types and functions.
```haskell
data User
  = User { name :: String
         , age  :: Int
         }

birthday :: User -> User
birthday user = user{age = user.age + 1}
```
The Swift side should wrap Haskell's `birthday`:
```swift
struct User {
    let name: String
    let age: Int
}

// birthday(user: User(name: "Anton", age: 33)) = User(name: "Anton", age: 34)
func birthday(user: User) -> User {
    // Calls Haskell function...
}
```

To support this workflow, we need a way to **convert the User datatype from
Haskell to Swift**, and vice versa. We are going to **serialize (most) inputs
and outputs** of a function. Even though the serialization as it will be
described may seem complex, it can be automated with Template Haskell and Swift
Macros and packed into a neat interface -- which I've done in
[haskell-swift](https://github.com/alt-romes/haskell-swift).

<!-- -- though, in a follow up post, I will also dive a -->
<!-- bit into coercing in-memory representations of a datatype in between Haskell and -->
<!-- Swift -- which, unlike serializing the datatypes, is very fragile, but -->
<!-- educational. -->

As a preliminary step, we add the `User` data type and `birthday` function to
`haskell-framework/src/MyLib.hs`, and the Swift equivalents to
`SwiftHaskell/ContentView.swift` from the [`haskell-x-swift-project-steps`](https://github.com/alt-romes/haskell-x-swift-project-steps) example project.

# Marshaling Inputs and Outputs

Marshaling the inputs and outputs of a function, from the Swift perspective,
means to serialize the input values into strings, and receive the output value as
a string which is then decoded into a Swift value. The Haskell perspective is
dual.

Marshaling/serializing is a very robust solution to foreign language interoperability.
While there is a small overhead of encoding and decoding at a function call, it
almost automatically extends to, and enables, all sorts of data to be
transported across the language boundary, without it being vulnerable to
compiler implementation details and memory representation incompatibilities.

We will use the same marshaling strategy that [Calling Purgatory from Heaven: Binding to Rust in Haskell](https://well-typed.com/blog/2023/03/purgatory) does.
In short, the idiomatic Haskell function is wrapped by a low-level one which
deserializes the Haskell values from the argument buffers, and serializes the
function result to a buffer that the caller provides. More specifically,

* For each argument of the original function, we have a `Ptr CChar` and `Int` -- a string of characters and the size of that string (a.k.a `CStringLen`)
* For the result of the original function, we have two additional arguments, `Ptr CChar` and `Ptr Int` --
    an empty buffer in memory, and a pointer to the size of that buffer, both allocated by the caller.
* For each argument, we parse the C string into a Haskell value that serves as an argument to the original function.
* We call the original function
* We overwrite the memory location containing the original size of the buffer with the *required* size of the buffer to fit the result (which may be smaller or larger than the actual size).
    If the buffer is large enough we write the result to it.
* From the Swift side, we read the amount of bytes specified in the memory
    location that now contains the *required* size. If it turns out that the
    *required size* is larger than the buffer's size, we need to retry the
    function call with a larger buffer.

  - This means we might end up doing the work twice, if the original buffer size
    is not big enough. Some engineering work might allow us to re-use the
    result, but we'll stick with retrying from scratch for simplicity.

We will use `JSON` as the serialization format: this choice is motivated
primarily by convenience because Swift can derive JSON instances for datatypes
out of the box (without incurring in extra dependencies), and in Haskell we can
use `aeson` to the same effect. In practice, it could be best to use a format
such as CBOR or Borsh which are binary formats optimised for compactness and
serialization performance.

## Haskell's Perspective

Extending the `User` example requires `User` to be decodable, which can be done automatically by adding to the `User` declaration:

```haskell
deriving stock Generic
deriving anyclass (ToJSON, FromJSON)
```

With the appropriate extensions and importing the necessary modules in `MyLib`:

```haskell
{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

-- ...

import GHC.Generics
import Data.Aeson
```

The `MyForeignLib` module additionally must import

```haskell
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Data.Aeson
import Data.ByteString
import Data.ByteString.Unsafe
```

Now, let's (foreign) export a function `c_birthday` that wraps
`birthday` above in `haskell-framework/flib/MyForeignLib.hs`, using the
described method.

First, the type definition of the function receives the buffer with the `User` argument, and a
buffer to write the `User` result to. We cannot use tuples because they are not
supported in foreign export declarations, but the intuition is that the first
two arguments represent the original `User` input, and the two latter arguments
represent the returned `User`.

```haskell
c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()
```

Then, the implementation -- decode the argument, encode the result, write
result size to the given memory location and the result itself to the buffer, if
it fits.

```haskell
c_birthday cstr clen result size_ptr = do
```

We transform the `(Ptr CChar, Int)` pair into a `ByteString` using
`unsafePackCStringLen`, and decode a `User` from the `ByteString` using
`decodeStrict`:
```haskell
  -- (1) Decode C string
  Just user <- decodeStrict <$> unsafePackCStringLen (cstr, clen)
```

We apply the original `birthday` function to the decoded `user`. In our example,
this is a very boring function, but in reality this is likely a complex
idiomatic Haskell function that we want to expose to.
```haskell
  -- (2) Apply `birthday`
  let user_new = birthday user
```

We encode the `new_user :: User` as a `ByteString`, and use
`unsafeUseAsCStringLen` to get a pointer to the bytestring data and its length.
Finally, we get the size of the result buffer, write the actual size of the
result to the given memory location, and, if the actual size fits the buffer,
copy the bytes from the bytestring to the given buffer.
```haskell
  -- (3) Encode result
  unsafeUseAsCStringLen (toStrict $ encode user_new) $ \(ptr,len) -> do

    -- (3.2) What is the size of the result buffer?
    size_avail <- peek size_ptr

    -- (3.3) Write actual size to the int ptr.
    poke size_ptr len

    -- (3.4) If sufficient, we copy the result bytes to the given result buffer
    if size_avail < len
       then do
         -- We need @len@ bytes available
         -- The caller has to retry
         return ()
       else do
         moveBytes result ptr len
```
If the written *required* size is larger than the given buffer, the caller will
retry.

Of course, we must export this as a C function.
```haskell
foreign export ccall c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()
```
This makes the `c_birthday` function wrapper available to Swift in the generated
header and at link time in the dynamic library.

## Swift's Perspective

In Swift, we want to be able to call the functions exposed from Haskell via
their C wrappers from a wrapper that feels idiomatic in Swift. In our example,
that means wrapping a call to `c_birthday` in a new Swift `birthday` function.

In `ContentView.swift`, we make `User` JSON-encodable/decodable by conforming to
the `Codable` protocol:
```swift
struct User: Codable {
    // ...
}
```

Then, we implement the Swift side of `birthday` which *simply* calls
`c_birthday` -- the whole logic of `birthday` is handled by the Haskell side
function (recall that `birthday` could be incredibly complex, and other
functions exposed by Haskell will indeed be).
```swift
func birthday(user: User) -> User {
    // ...
}
```

Note: in the implementation, a couple of blocks have to be wrapped with a `do {
... } catch X { ... }` but I omit them in this text. You can see the commit
relevant to the Swift function wrapper implementation in the repo with all of
these details included.

First, we encode the Swift argument into JSON using the `Data` type (plus its
length) that will serve as arguments to the foreign C function.
```swift
let enc = JSONEncoder()
let dec = JSONDecoder()

var data: Data = try enc.encode(user)
let data_len = Int64(data.count)
```

However, a Swift `Data` value, which represents the JSON as binary data, cannot
be passed directly to C as a pointer. For that, we must use
`withUnsafeMutableBytes` to get an `UnsafeMutableRawBufferPointer` out of the
`Data` -- that we can pass to the C foreign function. `withUnsafeMutableBytes`
receives a closure that uses an `UnsafeMutableRawBufferPointer` in its scope and
returns the value returned by the closure. Therefore we can return the result of
calling it on the user `Data` we encoded right away:

```swift
return data.withUnsafeMutableBytes { (rawPtr: UnsafeMutableRawBufferPointer) in
    // here goes the closure that can use the raw pointer,
    // the code for which we describe below
}
```

We allocate a buffer for the C foreign function to insert the result of
calling the Haskell function, and also allocate memory to store the size of the
buffer. We use `withUnsafeTemporaryAllocation` to allocate a buffer that can be
used in the C foreign function call. As for `withUnsafeMutableBytes`, this
function also takes a closure and returns the value returned by the closure:

```swift
// The data buffer size
let buf_size = 1024048 // 1024KB

// A size=1 buffer to store the length of the result buffer
return withUnsafeTemporaryAllocation(of: Int.self: 1) { size_ptr in
    // Store the buffer size in this memory location
    size_ptr.baseAddress?.pointee = buf_size

    // Allocate the buffer for the result (we need to wrap this in a do { ...} catch for reasons explained below)
    do {
        return withUnsafeTemporaryAllocation(byteCount: buf_size, alignment:1) { res_ptr in

            // Continues from here ...
        }
    } catch // We continue here in due time ...
}
```

We are now nested deep within 3 closures: one binds the pointer to the
argument's data, the other the pointer to the buffer size, and the other the
result buffer pointer. This means we can now call the C foreign function
wrapping the Haskell function:
```swift
c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)
```

Recalling that the Haskell side will update the size pointed to by `size_ptr` to
the size required to serialize the encoded result, we need to check if
this required size exceeds the buffer we allocated, or read the data otherwise:

```swift
if let required_size = size_ptr.baseAddress?.pointee {
    if required_size > buf_size {
        // Need to try again
        throw HsFFIError.requiredSizeIs(required_size)
    }
}

return dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!,
                    count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
```

where `HsFFIError` is a custom error defined as
```swift
enum HsFFIError: Error {
    case requiredSizeIs(Int)
}
```
We must now fill in the `catch` block to retry the foreign function call with a
buffer of the right size:
```swift
} catch HsFFIError.requiredSizeIs(let required_size) {
    return withUnsafeTemporaryAllocation(byteCount: required_size, alignment:1)
    { res_ptr in
        size_ptr.baseAddress?.pointee = required_size
        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

        return dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!,
                    count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
    }
}
```

That seems like a lot of work to call a function from Haskell! However, despite
this being a lot of code, not a whole lot is happening: we simply serialize the
argument, allocate a buffer for the result, and deserialize the result into it.
In the worst case, if the serialized result does not fit (the serialized data
has over 1M characters), then we *naively* compute the function a
second time (it should not be terribly complicated to avoid this work by caching
the result and somehow resuming the serialization with the new buffer).
Furthermore, there is a lot of bureocracy in getting the raw pointers to send
off to Haskell land -- the good news is that all of this can be automated away
behind automatic code generation with Template Haskell and Swift Macros.

<details>
<summary>Expand for the complete function</summary>

```swift
func birthday (user : User) -> User {
    let enc = JSONEncoder()
    let dec = JSONDecoder()
    do {
        var data : Data = try enc.encode(user)
        let data_len = Int64(data.count)
        return try data.withUnsafeMutableBytes { (rawPtr:UnsafeMutableRawBufferPointer) in

            // Allocate buffer for result
            let buf_size = 1024000

            return try withUnsafeTemporaryAllocation(of: Int.self, capacity: 1) { size_ptr in
                size_ptr.baseAddress?.pointee = buf_size

                do {
                    return try withUnsafeTemporaryAllocation(byteCount: buf_size, alignment: 1) {
 res_ptr in

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        if let required_size = size_ptr.baseAddress?.pointee {
                            if required_size > buf_size {
                                throw HsFFIError.requiredSizeIs(required_size)
                            }
                        }
                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                } catch HsFFIError.requiredSizeIs(let required_size) {
                    print("Retrying with required size: \(required_size)")
                    return try withUnsafeTemporaryAllocation(byteCount: required_size, alignment:
 1) { res_ptr in
                        size_ptr.baseAddress?.pointee = required_size

                        c_birthday(rawPtr.baseAddress, data_len, res_ptr.baseAddress, size_ptr.baseAddress)

                        return try dec.decode(User.self, from: Data(bytesNoCopy: res_ptr.baseAddress!, count: size_ptr.baseAddress?.pointee ?? 0, deallocator: .none))
                    }
                }
            }
        }
    } catch {
        print("Error decoding JSON probably: \(error)")
        return User(name: "", age: 0)
    }
}
```

</details>

We can test that this is working by replacing `ContentView` with:

```swift
struct ContentView: View {
    var body: some View {
        VStack {
            let user = birthday(user: User(name: "Ellie", age: 24))
            Text("Post-birthday, \(user.name) is: \(user.age)!")
        }
        .padding()
    }
}
```

And you should see:

![Fig 1. Swift app displays result of calling idiomatic Haskell function via idiomatic Swift wrapper](/images/calling-haskell-from-swift/ss1.jpeg)


# Metaprogramming at the boundaries

I want to give a quick preview of what is made possible by using compile-time
code generation features (Template Haskell in Haskell, Swift Macros in Swift).
This foreign function code generation API is exposed by the
[haskell-swift](https://github.com/alt-romes/haskell-swift) project, namely the
`swift-ffi` Haskell library and `haskell-ffi` Swift package (since it is out of
the scope of this tutorial, I will not cover how exactly the compile-time
code-generation code works, but instead use the API provided by these
libraries).

With these top-level foreign interaction facilities, coupled with the build tool also
provided by [haskell-swift](https://github.com/alt-romes/haskell-swift), one can
easily bootstrap and develop programs mixing Haskell and Swift.

Let us consider the same example where we define an idiomatic `birthday :: User
-> User` function in Haskell and want to be able to call it from Swift as
`birthday(user: User) -> User`

## Haskell's perspective

To expose the `birthday` function to Swift, we simply use the `foreignExportSwift`
Template Haskell function. The whole module could look like this:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module MyLib where

-- ...
import Swift.FFI

data User
 = User { name :: String
        , age  :: Int
        }
        deriving stock    Generic
        deriving anyclass FromJSON
        deriving anyclass ToJSON

birthday :: User -> User
birthday User{age=x, name=y} = User{age=x+1, name=y}

$(foreignExportSwift 'birthday)
```
The key bit is the last `foreignExportSwift` call which will expose a C function
with the marshalling-based calling convention we outlined above.

## Swift's perspective

On the Swift side, we want to use the dual `@ForeignImportHaskell` macro which
generates a Swift function wrapper which in turn invokes the C function exposed
by Haskell with the above marshalling strategy. The Swift file could
look like:

```swift
import HaskellFFI

struct User: Codable {
    let name: String
    let age: Int
}

@ForeignImportHaskell
func birthday(cconv: HsCallJSON, user: User) -> User { stub() }
```

where `birthday` could be called e.g. as:
```swift
birthday(user: User(name: "Pierre", age: 55))
```

# Remarks

The strategy of marshaling for foreign language boundary crossing is very robust
and still performant, and is a great fit for the kind of mixed-language
application we want to develop robustly.

Even though marshaling is required for robustly traversing the foreign language
boundary, I will also explore, in a subsequent post, calling Haskell from Swift
by instead coercing the memory representation of a Haskell value into a Swift
one -- this will mostly be a (very unsafe) and not-at-all robust curiosity, but
it will give me an excuse to write a bit about low-level details in Haskell!

In yet another post, I also intend to introduce the `hxs` tool for bootstrapping
Haskell x Swift projects and the libraries that make it so much easier to export
Haskell functions and import them from Swift.

The [haskell-x-swift-project-steps](https://github.com/alt-romes/haskell-x-swift-project-steps)
git repository has a commit matching the steps of this guide, so if anything is
unclear you can just let the code speak by itself in checking the commits.

This project, blog post, and research regarding Swift interoperability with
Haskell is being partially sponsored by [Well-Typed](https://well-typed.com/),
and is otherwise carried out in my own free time. If you'd also like to sponsor my
work on Swift x Haskell interoperability with the goal of developing native
macOS/iOS/etc applications, visit [my GitHub sponsors page](https://github.com/sponsors/alt-romes).



