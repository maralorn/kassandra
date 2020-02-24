{ system ? builtins.currentSystem }:
(import (import ./nix/sources.nix).reflex-platform {
  inherit system;
  config = { allowBroken = true; };
}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = [ "common" "backend" "frontend" ];
    ghcjs = [ "common" "frontend" ];
  };
})
