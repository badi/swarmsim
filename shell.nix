{ pkgs ? import ./nixpkgs.nix, compiler ? "ghc801" }:

with pkgs;

let
   ghc = haskell.compiler."${compiler}";
   hs = haskell.packages."${compiler}";
in

haskell.lib.buildStackProject {
  name = "SwarmSim";
  buildInputs =
    [zlib SDL SDL_image SDL_ttf glfw freeglut mesa_glu ]
    ++
    [ blas liblapack ]
    ++
    (with hs;
    [cabal-install])
    ++
    (with xorg;
    [libX11 libXi libXrandr libXxf86vm libXcursor libXinerama libXext])
    ;
  inherit ghc;
}
