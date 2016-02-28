{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
   myemacs =
     with pkgs.emacsPackages; emacsWithImageMagick.emacsWithPackages
             [ haskellMode magit emacsw3m ];
   myhaskell =
     pkgs.haskellPackages.ghcWithPackages (p: with p; [
        array base binary comonad containers dlist free mtl parsec QuickCheck
        semigroups semigroupoids cabal-install
    ]);
in with pkgs; stdenv.mkDerivation {
  name = "Philed";
  buildInputs = [ myhaskell ];
}
#TODO: Dependency on which should be made runtime.
