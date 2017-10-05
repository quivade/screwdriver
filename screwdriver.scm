(use-modules (guix)
             (guix build-system haskell)
             (guix licenses)
             (gnu packages haskell)
             (nat guix packages haskell))

(package
  (name "screwdriver")
  (version "0.0.1.0")
  (source (local-file "./."))
  (build-system haskell-build-system)
  (native-inputs
  `(("cabal-install", cabal-install)))
  (inputs
    `(("ghc-mtl" ,ghc-mtl)
      ("ghc-parsers" ,ghc-parsers)
      ("ghc-prettyprinter" ,ghc-prettyprinter)
      ("ghc-text" ,ghc-text)
      ("ghc-trifecta" ,ghc-trifecta)
      ("ghc-unification-fd" ,ghc-unification-fd)
      ("ghc-unordered-containers"
      ,ghc-unordered-containers)
      ("ghc-tasty" ,ghc-tasty)
      ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
      ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
      ("ghc-test-invariant" ,ghc-test-invariant)
      ("ghc-quickcheck" ,ghc-quickcheck)
      ("ghc-smallcheck" ,ghc-smallcheck)))
  (home-page
    "http://github.com/jkopanski/screwdriver#readme")
  (synopsis "Utility toolset for HDL manipulation")
  (description "Please see README.md")
  (license gpl3))
