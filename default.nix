{ obelisk ? import ./.obelisk/impl {
  system = builtins.currentSystem;
  iosSdkVersion = "10.2";
  # You must accept the Android Software Development Kit License Agreement at
  # https://developer.android.com/studio/terms in order to build Android apps.
  # Uncomment and set this to `true` to indicate your acceptance:
  # config.android_sdk.accept_license = false;
} }:
with obelisk;
project ./. ({ pkgs, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super: {
    clay =
      pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.dontCheck super.clay);
    taskwarrior = self.callCabal2nix "taskwarrior" ../haskell-taskwarrior { };
    backend = pkgs.haskell.lib.addBuildDepend super.backend pkgs.taskwarrior;
  };
  shellToolOverrides = ghc: _:
    let
      overrides = ghc: _: {
        haddock-library = pkgs.haskell.lib.dontCheck
          (ghc.callHackage "haddock-library" "1.8.0" { });
        haskell-lsp = ghc.callHackage "haskell-lsp" "0.19.0.0" { };
        haskell-lsp-types = ghc.callHackage "haskell-lsp-types" "0.19.0.0" { };
        regex-posix = ghc.callHackage "regex-posix" "0.96.0.0" { };
        test-framework = pkgs.haskell.lib.dontCheck
          (ghc.callHackage "test-framework" "0.8.2.0" { });
        regex-base = ghc.callHackage "regex-base" "0.94.0.0" { };
        regex-tdfa = ghc.callHackage "regex-tdfa" "1.3.1.0" { };
        shake =
          pkgs.haskell.lib.dontCheck (ghc.callHackage "shake" "0.18.4" { });
        hie-bios = pkgs.haskell.lib.dontCheck (ghc.callHackageDirect {
          pkg = "hie-bios";
          ver = "0.4.0";
          sha256 = "19lpg9ymd9656cy17vna8wr1hvzfal94gpm2d3xpnw1d5qr37z7x";
        } { });
        ghcide = pkgs.haskell.lib.dontCheck (ghc.callHackageDirect {
          pkg = "ghcide";
          ver = "0.1.0";
          sha256 = "0vwaaqb74dzsvx5xdfkzbi8zzvbd5w9l1wdhl3rhvi8ibnrchgfs";
        } { });
      };
      ghcidePkgSet = ghc.override { inherit overrides; };
    in { inherit (ghcidePkgSet) ghcide; };
  packages = { standalone = ./standalone; };
})
