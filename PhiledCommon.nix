{ mkDerivation, array, base, binary, comonad, containers, free, mtl
, parsec, QuickCheck, semigroupoids, semigroups, semiring-simple
, stdenv, transformers, fetchFromGitHub
}:
mkDerivation {
  pname = "PhiledCommon";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "Chattered";
    repo = "PhiledCommon";
    rev = "master";
    sha256 = "1jib7wq9f4sw8yr01iiscpm8z2x8pyv21as05pnqd93zmb9gvzf2";
  };
  libraryHaskellDepends = [
    array base binary comonad containers free mtl parsec QuickCheck
    semigroupoids semigroups semiring-simple transformers
  ];
  description = "Ad-hoc utilities.";
  license = stdenv.lib.licenses.mit;
}
