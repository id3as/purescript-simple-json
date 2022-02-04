{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-kernel"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "datetime"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "partial"
  , "prelude"
  , "record"
  , "transformers"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
