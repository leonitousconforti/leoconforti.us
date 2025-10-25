---

title: Computed Properties for Haskell Records

description: Adding computed properties to Haskell record types -- probably a
             new Haskell anti-pattern!

tags: haskell, records

---

# Records in Haskell

Haskell has so-called record types, which are also commonly known as structs,
for instance, in C, Swift, and Rust. To define a square, one would write:
```haskell
data Point
  = Point
    { x :: Int
    , y :: Int
    }

data Square
  = Square
    { topLeft     :: Point
    , bottomRight :: Point
    }

mySquare = Square{ topLeft = Point{x = 0, y = 0}
                 , bottomRight = Point{x = 2, y = 2} }
mySquareWidth = x (bottomRight mySquare) - x (topLeft mySquare)
```
In Haskell record types are just syntactic sugar for ordinary product types
paired with functions that get and set these fields.
In essence, the above is not fundamentally different from having the following
standard product types and functions:
```haskell
data Point = Point Int Int
data Square = Square Point Point

x, y :: Point -> Int
x (Point px _) = px
y (Point _ py) = py

topLeft, bottomRight :: Square -> Point
topLeft (Square tl _) = tl
bottomRight (Square _ br) = br

-- And setters...
```

## Overloaded Record Dot

However, by turning on the `OverloadedRecordDot` syntax extension, you can
use more syntactic sugar to project the fields of a record instead of using the
field name as a standard function:
```haskell
{-# LANGUAGE OverloadedRecordDot #-}
mySquareWidth = mySquare.bottomRight.x - mySquare.topLeft.x
```
which is neat!
I like `OverloadedRecordDot`. It looks clean and feels more like using proper
property of the record data type. It is also less ambiguous for an LSP to
suggest the record properties of a data type by typing after the `.`, than it is
to suggest functions to apply to the record type argument.

## Named Field Puns

Since I'm already writing about records, I'll mention another extension I quite
enjoy: `NamedFieldPuns`.

Traditionally, when matching on a record, you can list the field names and bind
variables to the value associated with that field. Continuing the above example:
```haskell
area :: Square -> Int
area Square{topLeft = tl, bottomRight = br}
    = (br.x - tl.x) * (br.y - tl.y)
```
We know, however, that naming things is hard and best avoided. With
`NamedFieldPuns`, instead of declaring the variable to be bound to the right of
the field name, we have the field name be the variable bound to its value:
```haskell
area :: Square -> Int
area Square{topLeft, bottomRight}
    = (bottomRight.x - topLeft.x) * (bottomRight.y - topLeft.y)
```
There are more record-related extensions, such as `RecordWildCards` or
`OverloadedRecordUpdate`, which I will not get into, but that can also make life
smoother when working with records.


# Computed Properties

Computed properties are a (not-particularly-exclusive-to) Swift concept I've
recently come accross while working on my [interoperability between Haskell and
Swift project](2023-11-10-creating-a-macos-app-with-haskell-and-swift.html).

Here is a definition from the [Swift book section on (computed)
properties](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/properties/):

> *Properties* associate values with a particular class, structure, or
> enumeration. Stored properties store constant and variable values as part of
> an instance, whereas computed properties calculate (rather than store) a
> value. Computed properties are provided by classes, structures, and
> enumerations. Stored properties are provided only by classes and structures.

And a Swift example, where the `volume` is a property computed from the `width`,
the `height` and the `depth` of a `Cuboid`:
```swift
struct Cuboid {
    var width = 0.0, height = 0.0, depth = 0.0
    var volume: Double {
        return width * height * depth
    }
}
```
C# also has a notion of [computed properties](https://learn.microsoft.com/en-us/dotnet/csharp/properties#computed-properties).
In Java, simple class methods computing a result from class properties can also
be seen as some sort of computed property, or, really, class methods in any
object oriented language.

In Haskell, as basically everything else, you can think of computed properties
as... just functions. But there is one key element to computed properties that
makes them different from just functions -- them being called using dot syntax
at a value of a record type just like any other property, and the evoking the
idea of describing a *property* of the record type.

Can we have that kind of computed properties in Haskell? Well, of course!

Following the examples in previous sections, consider the `area` function to be
conceptually a *computed property* of a rectangle, as it uses the two `Square`
properties (`topLeft` and `bottomRight`) to compute a new one. Ultimately, we
want to be able to type:
```haskell
> mySquare.area
6
```
To achieve this, we need to emulate the behaviour of field accessors. The key
insight is to use the `HasField` class just like default field accessors do.
`HasField` enables so-called [record field selector
polymorphism](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html)
and allows us to not only define functions to operate on any record type with eg a
field named `name`, it allows us to define field accessors for non-record types,
and, ultimately, allows us to create computed properties. The ability to
define our own instances of `HasField` is also documented in the user guide
under [virtual record fields](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html#virtual-record-fields).

To make the `area` function a field accessor, thereby making it a
record-dot-enabled-computed-property, we instance `HasField` using the field
name `area` (which is a defined as a type-level string in the first argument to
`HasField`):
```haskell
{-# LANGUAGE GHC2021, OverloadedRecordDot, DataKinds #-}
import GHC.Records

instance HasField "area" Square Int where
    getField = area
```
You can now write `some_square.area` to compute the area of the square based on
its record properties.

Here's an example of a full program defining another computed property and printing it:
```haskell
{-# LANGUAGE GHC2021, OverloadedRecordDot, DataKinds #-}

import GHC.Records

data User = User
  { birthYear :: Int
  , name :: String
  }

instance HasField "age" User Int where getField = age

age :: User -> Int
age u = 2023 - u.birthYear

user :: User
user = User{birthYear=1995, name="Robert"}

main = print user.age
```

# Conclusion

This has been a whirlwhind thought kind of post. I am not currently using this
in any project. I thought "I suppose we can also have this neat properties
sugar" and tried it, this is only my exposition of the idea.

In my opinion, this could be handy as some functions can really be better thought of as
properties of a datatype, and doing so doesn't preclude you from also using it
as a function in cases where it reads more naturally (and of course, pass it on
to higher order functions). LSP based autocompletion of (computed) properties after the dot might be another positive factor.
It is probably also a Haskell anti-pattern for some!

I'm left wondering:
is anyone out there doing this?

