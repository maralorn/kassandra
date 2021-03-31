let Prelude = https://prelude.dhall-lang.org/v16.0.0/package.dhall

let extensions =
      [ "AllowAmbiguousTypes"
      , "BlockArguments"
      , "ConstraintKinds"
      , "DataKinds"
      , "DeriveAnyClass"
      , "DeriveGeneric"
      , "DerivingStrategies"
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
      [ "-Wall"
      , "-Wcompat"
      , "-Wno-orphans"
      , "-Wincomplete-uni-patterns"
      , "-Wincomplete-record-updates"
      , "-Wmissing-export-lists"
      , "-Widentities"
      , "-Wredundant-constraints"
      , "-Wmissing-home-modules"
      ]

let multiLineList =
      Prelude.Text.concatMapSep "\n" Text (λ(x : Text) → "    ${x}")

in  ''
    -- BEGIN dhall generated common configuration
    -- generate with: dhall text --file common-config.dhall
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
