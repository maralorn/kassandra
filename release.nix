{ pkgs ? import <nixpkgs-master> { } }:
let
  kassandra = pkgs.haskellPackages.callCabal2nix "kassandra" ./kassandra { };
  standalone = pkgs.haskellPackages.callCabal2nix "standalone" ./standalone { inherit kassandra; };
  reflex-platform = import ./. {};
in {
  inherit standalone;
  server = reflex-platform.exe;
  androidApk = reflex-platform.android.frontend;
}
