
[![build]](https://github.com/phile314/pinch-gen)

`pinch` aims to provide an alternative implementation of Apache Thrift for
Haskell. The `pinch` library itself acts only as a serialization library. Types
specify their Thrift encoding by defining instances of the `Pinchable`
typeclass, which may be done by hand or automatically with the use of Generics.

  [build]: https://github.com/phile314/pinch-gen/workflows/build/badge.svg

Haddock documentation for this package is avilable on [Hackage].

  [Hackage]: http://hackage.haskell.org/package/pinch-gen

Overview
--------

Converts a Thrift file into Haskell code for the pinch library.

Usage:

```
Usage: pinch-gen --in IN_FILE --out OUT_DIR --hashable-vec-mod ARG
                 [--no-generate-arbitrary] [--no-generate-nfdata] [--extra-import IMPORT]
  Generate Haskell files from a thrift input file.
```

The `--hashable-vec-mod` argument should be set to a module providing a `Hashable` instance for Vector.
This is required as a Vector may become part of a key of a map, but neither the vector nor the hashable
package provide an instance. For some background, see https://github.com/haskell/vector/pull/102 .The simplest
solution is to depend on the `vector-instances` package and pass `--hashable-vec-mod Data.Vector.Instances` to
pinch-gen.

Compatibility
-------------

| pinch version | pinch-gen version |
|---------------|-------------------|
| 0.4           | 0.4.*             |


Example
-------

Let us use this simple Thrift service as an example:

#### **`trivial.thrift`**
```
# A simple struct
struct MyStruct {
  1: required binary payload;
}

# Trivial exception for testing only.
exception Exception {

  # The exception simply contains a message string.
  1: required string message;
}

# Trivial service for testing only.
service Trivial {

  # Takes a struct and returns a string.
  string success(1: MyStruct argument);

  # Throws an arbitrary string.
  void failure() throws (1: Exception error);

  # Fire rocket.
  oneway void fireAndForget(1: i32 rocket);
}
```

To generate the corresponding Haskell code we can call pinch-gen:

```
pinch-gen --no-generate-arbitrary --no-generate-nfdata --hashable-vec-mod Data.Vector.Instances --in trivial.thrift --out out/
```

This will create the appropriate datatypes for all struct, union and exception types:


#### **`out/Trivial/Types.hs`**
```
data MyStruct
  = MyStruct
  { myStruct_payload :: Data.ByteString.ByteString
  }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)

data Exception
  = Exception
  { exception_message :: Data.Text.Text
  }
  deriving (Prelude.Eq, GHC.Generics.Generic, Prelude.Show)
```

For the server, a record-style encoding of all functions is used. Given an implementation of these functions,
a `Pinch.Server.ThriftServer` for use with the pinch library can be created:


#### **`out/Trivial/Server.hs`**
```
data Trivial
  = Trivial
  { success :: (Pinch.Server.Context) -> (MyStruct) -> (Prelude.IO Data.Text.Text)
  , failure :: (Pinch.Server.Context) -> (Prelude.IO ())
  , fireAndForget :: (Pinch.Server.Context) -> (Data.Int.Int32) -> (Prelude.IO ())
  }

trivial_mkServer :: (Trivial) -> Pinch.Server.ThriftServer
trivial_mkServer server = ...
```

For the client, functions creating a `Pinch.Client.ThriftCall` for use with the pinch library are generated.
You can use `Pinch.Client.call` if you want to explicitly match on the success result/thrown exceptions as defined
in the Thrift file. Alternatively, you may use `Pinch.Client.callOrThrow` to directly access the result. In case
the rqeuest failed, `callOrThrow` will throw an exception using `throwIO`.


#### **`out/Trivial/Client.hs`**
```
success :: (MyStruct) -> (Pinch.Client.ThriftCall Success_Result)
success argument = ...

failure :: (Pinch.Client.ThriftCall Failure_Result)
failure  = ...

fireAndForget :: (Data.Int.Int32) -> (Pinch.Client.ThriftCall Pinch.Internal.RPC.Unit)
fireAndForget rocket = ...
```

Caveats
-------

The generated code is currently not formatted very nicely.
