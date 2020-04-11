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
    taskwarrior = self.callHackageDirect {
      pkg = "taskwarrior";
      ver = "0.1.2.3";
      sha256 = "12l6s8dm2cs81bhcf6g84imnk9239bv4j0q52saac2axm0ixxznx";
    } { };
    backend = pkgs.haskell.lib.addBuildDepend super.backend pkgs.taskwarrior;
    indexed-profunctors = self.callHackageDirect {
      pkg = "indexed-profunctors";
      ver = "0.1";
      sha256 = "0vpgbymfhnvip90jwvyniqi34lhz5n3ni1f21g81n5rap0q140za";
    } { };
    generic-lens-core = self.callHackageDirect {
      pkg = "generic-lens-core";
      ver = "2.0.0.0";
      sha256 = "07parw0frqxxkjbbas9m9xb3pmpqrx9wz63m35wa6xqng9vlcscm";
    } { };
    generic-optics = self.callHackageDirect {
      pkg = "generic-optics";
      ver = "2.0.0.0";
      sha256 = "0xy5k5b35w1i1zxy0dv5fk1b3zrd3hx3v5kh593k2la7ri880wmq";
    } { };
    optics-core = self.callHackageDirect {
      pkg = "optics-core";
      ver = "0.2";
      sha256 = "0ipshb2yrqwzj1prf08acwpfq2lhcrawnanwpzbpggdhabrfga2h";
    } { };
    optics-th = self.callHackageDirect {
      pkg = "optics-th";
      ver = "0.2";
      sha256 = "1hfvrdysp2hv8la682xpiywbk3407lshb9c99qzcc0grzm011hdg";
    } { };
    optics-extra = self.callHackageDirect {
      pkg = "optics-extra";
      ver = "0.2";
      sha256 = "03s20ybaqwfxwybmq20221any8xwv6c7nmzyqw8kaz3qm2zl86rz";
    } { };
    optics = self.callHackageDirect {
      pkg = "optics";
      ver = "0.2";
      sha256 = "17i8pzkcsv8dhpvzxgga1gfxwakmlanhac99lnvgf5ybg88a31yq";
    } { };
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
