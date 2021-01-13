
![build](https://github.com/phile314/pinch-gen/workflows/build/badge.svg)

`pinch` aims to provide an alternative implementation of Apache Thrift for
Haskell. The `pinch` library itself acts only as a serialization library. Types
specify their Thrift encoding by defining instances of the `Pinchable`
typeclass, which may be done by hand or automatically with the use of Generics.

Haddock documentation for this package is avilable on [Hackage].

  [Hackage]: http://hackage.haskell.org/package/pinch-gen

Overview
--------

Converts a Thrift file into Haskell code for the pinch library.


Caveats
-------

The generated code is currently not formatted very nicely.
