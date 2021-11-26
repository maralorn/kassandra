{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. (
  { pkgs, ... }:
  let
    inherit (pkgs.haskell.lib)
      markUnbroken dontCheck addBuildDepend doJailbreak
      overrideCabal
      ;
  in
  {
    android = {
      applicationId = "de.maralorn.kassandra";
      displayName = "Kassandra";
      releaseKey = null;
      isRelease = true;
      version = {
        code = import ./code.nix;
        name = "0.1.0";
      };
    };
    overrides = self: super: {
      kassandra = overrideCabal super.kassandra { doHaddock = false; };
      backend = addBuildDepend super.backend pkgs.taskwarrior;
      clay = markUnbroken (dontCheck super.clay);
      haskeline = dontCheck (self.callHackage "haskeline" "0.8.0.1" { });
      repline = doJailbreak (self.callHackage "repline" "0.4.0.0" { });
      dhall = dontCheck (self.callHackage "dhall" "1.35.0" { });
      relude = dontCheck super.relude;
      stm-containers = markUnbroken super.stm-containers;
      stm-hamt = markUnbroken (doJailbreak super.stm-hamt);
      streamly-bytestring = self.callHackageDirect
        {
          pkg = "streamly-bytestring";
          ver = "0.1.2";
          sha256 = "08xhp8zgf5n1j4v1br1dz9ih8j05vk92swp3nz9in5xajllkc7qv";
        }
        { };
      streamly = self.callHackageDirect
        {
          pkg = "streamly";
          ver = "0.7.0";
          sha256 = "0hr2cz14w6nnbvhnq1fvr8v4rzyqcj3b9khf2rszyji00fmp27l1";
        }
        { };
      nonempty-vector = self.callHackageDirect
        {
          pkg = "nonempty-vector";
          ver = "0.1.0.0";
          sha256 = "06abdmdy9z0w6ishiibir3qfjpqxmb4mrkhgyc4j58hd14s8rj0x";
        }
        { };
      nonempty-containers = self.callHackageDirect
        {
          pkg = "nonempty-containers";
          ver = "0.3.4.1";
          sha256 = "0nbnr0az201lv09dwcxcppkfc9b05kyw4la990z5asn9737pvpr2";
        }
        { };
      iCalendar = overrideCabal (doJailbreak (markUnbroken super.iCalendar)) {
        preConfigure = ''substituteInPlace iCalendar.cabal --replace "network >=2.6 && <2.7" "network -any"'';
      };
      prettyprinter = self.callHackageDirect
        {
          pkg = "prettyprinter";
          ver = "1.5.1";
          sha256 = "0wx01rvgwnnmg10sh9x2whif5z12058w5djh7m5swz94wvkg5cg3";
        }
        { };
      cborg-json = self.callHackageDirect
        {
          pkg = "cborg-json";
          ver = "0.2.2.0";
          sha256 = "1s7pv3jz8s1qb0ydcc5nra9f63jp4ay4d0vncv919bakf8snj4vw";
        }
        { };
      generic-random = self.callHackageDirect
        {
          pkg = "generic-random";
          ver = "1.3.0.0";
          sha256 = "0m7lb40wgmyszv8l6qmarkfgs8r0idgl9agwsi72236hpvp353ad";
        }
        { };
      atomic-write = self.callHackageDirect
        {
          pkg = "atomic-write";
          ver = "0.2.0.7";
          sha256 = "1r9ckwljdbw3mi8rmzmsnh89z8nhw2qnds9n271gkjgavb6hxxf3";
        }
        { };
      taskwarrior = self.callHackageDirect
        {
          pkg = "taskwarrior";
          ver = "0.5.0.0";
          sha256 = "sha256-elDUtz0NSG4WHxkyCQ1CunYXWIVRj6EqkKSchPy+c3E=";
        }
        { };
      base64 = self.callHackageDirect
        {
          pkg = "base64";
          ver = "0.4.1";
          sha256 = "1pz9s8bmnkrrr3v5mhkwv8vaf251vmxs87zzc5nsjsa027j9lr22";
        }
        { };
      password = self.callHackageDirect
        {
          pkg = "password";
          ver = "2.0.1.0";
          sha256 = "1q99v7w6bdfpnw245aa3zaj3x7mhl9i2y7f2rzlc30g066p9jhaz";
        }
        { };
      indexed-profunctors = self.callHackageDirect
        {
          pkg = "indexed-profunctors";
          ver = "0.1";
          sha256 = "0vpgbymfhnvip90jwvyniqi34lhz5n3ni1f21g81n5rap0q140za";
        }
        { };
      generic-lens-core = self.callHackageDirect
        {
          pkg = "generic-lens-core";
          ver = "2.0.0.0";
          sha256 = "07parw0frqxxkjbbas9m9xb3pmpqrx9wz63m35wa6xqng9vlcscm";
        }
        { };
      generic-optics = self.callHackageDirect
        {
          pkg = "generic-optics";
          ver = "2.0.0.0";
          sha256 = "0xy5k5b35w1i1zxy0dv5fk1b3zrd3hx3v5kh593k2la7ri880wmq";
        }
        { };
      optics-core = self.callHackage "optics-core" "0.3.0.1" { };
      optics-th = self.callHackage "optics-th" "0.3.0.2" { };
      optics-extra = self.callHackage "optics-extra" "0.3" { };
      optics = self.callHackage "optics" "0.3" { };
    };
    packages = {
      kassandra = ./kassandra;
      standalone = ./standalone;
    };
  }
)
