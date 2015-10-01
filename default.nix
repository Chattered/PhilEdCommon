{ haskellngPackages ? (import <nixpkgs> {}).haskellngPackages,
  pkgs ? (import <nixpkgs> {}).pkgs
}:
let
  env = haskellngPackages.ghcWithPackages (p: with p; [
    array base binary comonad containers dlist free mtl parsec QuickCheck
    semigroups semigroupoids cabal-install
  ]);
in pkgs.stdenv.mkDerivation {
  name = "Philed";
  buildInputs = [ env ];
}
