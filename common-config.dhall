let Prelude = https://prelude.dhall-lang.org/v16.0.0/package.dhall

let extensions =
      [ "AllowAmbiguousTypes"
      , "ConstraintKinds"
      , "DataKinds"
      , "DeriveAnyClass"
      , "DeriveGeneric"
      , "DuplicateRecordFields"
      , "EmptyCase"
      , "FlexibleContexts"
      , "FlexibleInstances"
      , "GADTs"
      , "LambdaCase"
      , "MultiParamTypeClasses"
      , "NamedFieldPuns"
      , "OverloadedLabels"
      , "OverloadedStrings"
      , "PartialTypeSignatures"
      , "PatternGuards"
      , "PatternSynonyms"
      , "QuasiQuotes"
      , "RankNTypes"
      , "RecursiveDo"
      , "ScopedTypeVariables"
      , "StandaloneDeriving"
      , "StrictData"
      , "TemplateHaskell"
      , "TupleSections"
      , "TypeApplications"
      , "TypeFamilies"
      , "UndecidableInstances"
      , "ViewPatterns"
      ]

let ghc-options =
      [ "-Weverything"
      , "-Wno-unsafe"
      , "-Wno-safe"
      , "-Wno-all-missed-specialisations"
      , "-Wno-implicit-prelude"
      , "-Wno-missed-specialisations"
      , "-Wno-monomorphism-restriction"
      , "-Wno-partial-fields"
      , "-Wno-missing-import-lists"
      , "-Wno-orphans"
      , "-Wcompat"
      ]

let multiLineList =
      Prelude.Text.concatMapSep "\n" Text (λ(x : Text) → "    ${x}")

in  ''
    -- BEGIN dhall generated common configuration
    license-file:       LICENSE
    author:             Malte Brandy
    maintainer:         malte.brandy@maralorn.de
    build-type:         Simple
    extra-source-files: CHANGELOG.md

    common common-config
      default-extensions:
    ${multiLineList extensions}
      ghc-options:
    ${multiLineList ghc-options}
      mixins:             base hiding (Prelude)
      default-language:   Haskell2010
    -- END dhall generated common configuration

    ''
