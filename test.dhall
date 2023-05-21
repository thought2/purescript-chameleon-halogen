{ name = "virtual-dom-halogen-test"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "console"
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
  , "arrays"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
