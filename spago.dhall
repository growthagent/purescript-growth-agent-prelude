let name = "growth-agent-prelude"

in  { name
    , license = "Apache-2.0"
    , repository = "https://github.com/instateam/purescript-${name}.git"
    , dependencies =
      [ "aff"
      , "bifunctors"
      , "const"
      , "control"
      , "debug"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "functors"
      , "gen"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "parallel"
      , "prelude"
      , "profunctor-lenses"
      , "safe-coerce"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "variant"
      ]
    , packages = ./packages.dhall
    , sources = [ "src/**/*.purs" ]
    }
