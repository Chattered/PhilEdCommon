{ pkgs ? (import <nixpkgs> {}).pkgs
}:
let
   philedcommon = import ./PhiledCommon.nix;
   myemacs =
     with pkgs.emacsPackages; with pkgs.emacsPackagesNg; pkgs.emacsWithPackages
      [ haskell-mode helm-projectile magit paredit ];
   myhaskell =
     pkgs.haskellPackages.ghcWithPackages (p: with p; [
     cabal-install hlint (pkgs.haskellPackages.callPackage philedcommon {})
    ]);
in with pkgs; stdenv.mkDerivation {
  name = "Philed";
  buildInputs = [ cabal2nix myemacs myhaskell ];
}
