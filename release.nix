{ pkgs ? import (import nix/sources.nix).nixpkgs {} }:
let
  inherit (pkgs.haskell.lib) overrideCabal doJailbreak unmarkBroken;
  haskellPackages = pkgs.haskellPackages.extend (
    self: super: {
      iCalendar = overrideCabal (doJailbreak (unmarkBroken super.iCalendar)) {
        preConfigure = ''substituteInPlace iCalendar.cabal --replace "network >=2.6 && <2.7" "network -any"'';
        #configureFlags = [ "--allow-newer=network" ]; # try this on ghc 9.0
      };
      stm-containers = unmarkBroken super.stm-containers;
      stm-hamt = unmarkBroken super.stm-hamt;
      primitive-extras = unmarkBroken super.primitive-extras;
    }
  );
  kassandra = haskellPackages.callCabal2nix "kassandra" ./kassandra {};
  standalone = haskellPackages.callCabal2nix "standalone" ./standalone {
    inherit kassandra;
  };
  reflex-platform = import ./. {};
in
{
  lib = kassandra;
  app = standalone;
  server = reflex-platform.exe;
  android = pkgs.runCommand "kassandra-android-apk" {} ''
    mkdir -p $out
    cp ${reflex-platform.android.frontend}/android-app-release-unsigned.apk $out/de.maralorn.kassandra_${
  import ./code.nix
  }.apk
  '';
}
