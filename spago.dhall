{ name = "virtual-dom-halogen"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "effect"
  , "foreign"
  , "foreign-object"
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
