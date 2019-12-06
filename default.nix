{ system ? builtins.currentSystem }:
(import (import ./nix/sources.nix).reflex-platform {
  inherit system;
  config = { allowBroken = true; };
}).project ({ pkgs, ... }: {
  overrides = let
    hoverrides = {
      #clay = "0.13.2";
      #hspec = "2.5.0";
      #hspec-discover = "2.5.0";
      #     lens = "4.17.1";
      #     microlens-th = "0.4.3.1";
      #     th-abstraction = "0.3.1.0";
      #     generic-deriving = "1.12.4";
      #     th-lift = "0.8";
      #     bifunctors = "5.5.4";
      #     invariant = "0.5.2";
      #     aeson = "1.4.3.0";
      #     webdriver = "0.9";
    };
  in self: super:
  (pkgs.lib.mapAttrs' (name: version:
    pkgs.lib.nameValuePair name (self.callHackage name version { })) hoverrides)
  // {
    clay = pkgs.haskell.lib.dontCheck super.clay;
  };
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
