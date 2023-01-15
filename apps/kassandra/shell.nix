{ pkgs ? import (import nix/sources.nix).nixpkgs { } }: let
  outputs = import ./release.nix { inherit pkgs; };
in pkgs.haskellPackages.shellFor {
  packages = (_: [outputs.lib outputs.app]);
}
