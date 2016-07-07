[![PureScript](logo.png)](http://purescript.org) - Erlang backend

PureScript is a small strongly typed programming language with expressive types that compiles to Javascript, written in and inspired by Haskell.

This fork is a PureScript backend targetting Erlang source. The [purerl](https://github.com/purerl) organisation hosts ports of some core libraries.

To use it:

```
pserlc 'bower_components/*/src/**/*.purs' 'src/**/*.purs'
erlc -o ebin/ output/*/*.erl
erl -pa ebin -noshell -eval '(main:main())()' -eval 'init:stop()'
```

# Output

Module names `Foo.Bar` are transformed to a lower-snake cased form `foo_bar` (any non-initial uppercase chars will be preserved as such).

Top level declarations are uniformly output as nullary functions. Identifiers are preserved, with quoting if required. Thus a normal invocation of the output will look like `(main:main())()`.

# Types

| PureScript type | Erlang type | Notes |
| --- | --- | --- |
| `Int` | `integer()` | Arbitrary precision - no longer a `Bounded` |
| `Number` | `float()` |
| `Boolean` | `boolean()` |
| `String` | `string()` |
| `Array` | `array()` | Not to be confused with erlang `[]` list syntax. |
| Records | |
| Tagged union | Tuple with tag element | e.g. `Some 42` is `{some, 42}` |
| Newtype | as underlying type |
| Functions | Function (arity 1 - but see FFI) |

# FFI
In place of `.js` FFI files, the Erlang backend has `.erl` FFI files. As per the regular compiler since 0.9, these must be placed along the corresponding `.purs` file with the same name.


Module name: `Foo.MyModule`
PureScript file `Foo/MyModule.purs`
Erlang file: `Foo/MyModule.erl`
Erlang module: `foo_myModule@foreign`

Note that the FFI code for a module must not only be in a file named correctly, but the module must be named the same as the output module with `@foreign` appended.

FFI files *MUST* export explicitly the exact set of identifiers which will be imported by the corresponding PureScript file. The compiler will check these exports and use them to inform codegen.

*Auto-currying*: functions can be defined with any arity. According to the arity of the export (parsed from the export list) `pserlc` will automatically apply to the right number of arguments. By extension, values are exported as a function of arity 0 returning that value.

An example:

```purescript
module Foo.Bar where

foreign import f :: Int -> Int -> Int -> Int
```

```erlang
-module(foo_bar@foreign).
-export([f/3]).

f(X, Y, Z) -> X + Y * Z.
```

This could also have been defined as
```erlang
-module(foo_bar@foreign).
-export([f/1]).

f(X) ->
  fun (Y) ->
    fun (Z) ->
      X + Y * Z
    end
  end.
```
