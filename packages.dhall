
let upstream =
  https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
    sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in upstream


with virtual-dom =
  { repo = "https://github.com/thought2/purescript-virtual-dom.git"
  , version = "c24b0a4986addfee1caf498b9268fc4bdb7232c7"
  , dependencies = [ "either", "foldable-traversable", "foreign", "maybe", "prelude", "strings", "these", "transformers", "tuples", "variant" ]
  }

