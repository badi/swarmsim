{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc801" }:

with pkgs;

let
   ghc = haskell.compiler."${compiler}";
   hs = haskell.packages."${compiler}";
in

haskell.lib.buildStackProject {
  name = "SwarmSim";
  buildInputs = [zlib hs.cabal-install];
  inherit ghc;
}
