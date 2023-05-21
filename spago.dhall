{ name = "virtual-dom-halogen"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "effect"
  , "foreign"
  , "halogen"
  , "prelude"
  , "safe-coerce"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  , "virtual-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
