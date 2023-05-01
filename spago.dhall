{ name = "virtual-dom-halogen"
, dependencies =
  [ "bifunctors"
  , "foreign"
  , "halogen"
  , "prelude"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  , "virtual-dom"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
