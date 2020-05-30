{ obelisk ? import ./.obelisk/impl {
  system = builtins.currentSystem;

  iosSdkVersion = "10.2";
  # You must accept the Android Software Development Kit License Agreement at
  # https://developer.android.com/studio/terms in order to build Android apps.
  # Uncomment and set this to `true` to indicate your acceptance:
  # config.android_sdk.accept_license = false;
} }:
with obelisk;
project ./. ({ pkgs, ... }:
  let
    inherit (pkgs.haskell.lib)
      markUnbroken dontCheck addBuildDepend doJailbreak;
  in {
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    overrides = self: super: {
      clay = markUnbroken (dontCheck super.clay);
      taskwarrior = self.callHackageDirect {
        pkg = "taskwarrior";
        ver = "0.2.0.0";
        sha256 = "1qpk0kh2ayr0nd8c6sang0ag2nm701plx0flr01hp21pjvifnpdg";
      } { };
      base64 = self.callHackageDirect {
        pkg = "base64";
        ver = "0.4.1";
        sha256 = "1pz9s8bmnkrrr3v5mhkwv8vaf251vmxs87zzc5nsjsa027j9lr22";
      } { };
      password = self.callHackageDirect {
        pkg = "password";
        ver = "2.0.1.0";
        sha256 = "1q99v7w6bdfpnw245aa3zaj3x7mhl9i2y7f2rzlc30g066p9jhaz";
      } { };
      backend = addBuildDepend super.backend pkgs.taskwarrior;
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
    __withGhcide = true;
    packages = { standalone = ./standalone; };
  })
