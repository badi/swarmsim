{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc801" }:

with pkgs;

let
   ghc = haskell.compiler."${compiler}";
   packages = haskell.packages."${compiler}";
in

haskell.lib.buildStackProject {
  name = "SwarmSim";
  buildInputs = with packages; [cabal-install];
  inherit ghc;
}
