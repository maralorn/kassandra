{ system ? builtins.currentSystem }:
(import (import ./nix/sources.nix).reflex-platform {
  inherit system;
  config = { allowBroken = true; };
}).project ({ pkgs, ... }: {
  overrides = self: super: {
    clay = pkgs.haskell.lib.dontCheck super.clay;
    taskwarrior = self.callCabal2nix "taskwarrior" ../haskell-taskwarrior { };
  };
  packages = { standalone = ./standalone; };

  shells = {
    ghc = [ "standalone" ];
    ghcjs = [ ];
  };
})
