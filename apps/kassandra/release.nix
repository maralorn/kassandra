{ pkgs ? import (import nix/sources.nix).nixpkgs { } }:
let
  haskellPackages = pkgs.haskellPackages.extend (
    self: super: {
      kassandra = self.callCabal2nix "kassandra" ./kassandra { };
      standalone = self.callCabal2nix "standalone" ./standalone { };
    }
  );
  reflex-platform = import ./. { };
in
{
  lib = haskellPackages.kassandra;
  app = haskellPackages.standalone;
  server = reflex-platform.exe;
  android = pkgs.runCommand "kassandra-android-apk" { } ''
    mkdir -p $out
    cp ${reflex-platform.android.frontend}/android-app-release-unsigned.apk $out/de.maralorn.kassandra_${import ./code.nix}.apk
  '';
}
