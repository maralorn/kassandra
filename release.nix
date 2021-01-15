{ pkgs ? import (import nix/sources.nix).nixpkgs { } }:
let
  kassandra = pkgs.haskellPackages.callCabal2nix "kassandra" ./kassandra { };
  standalone = pkgs.haskellPackages.callCabal2nix "standalone" ./standalone {
    inherit kassandra;
  };
  reflex-platform = import ./. { };
in {
  lib = kassandra;
  app = standalone;
  server = reflex-platform.exe;
  android = pkgs.runCommand "kassandra-android-apk" { } ''
    mkdir -p $out
    cp ${reflex-platform.android.frontend}/android-app-release-unsigned.apk $out/de.maralorn.kassandra_${
      import ./code.nix
    }.apk
  '';
}
