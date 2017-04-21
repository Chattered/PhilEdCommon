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
    rev = "83a827a061ab2291176ee79c991b9fb1b3924222";
    sha256 = "08d86rg0qx0p66mxn1wlz5jcpyfyynxafaj6bhsjdssgvlmrskrz";
  };
  libraryHaskellDepends = [
    array base binary comonad containers free mtl parsec QuickCheck
    semigroupoids semigroups semiring-simple transformers
  ];
  description = "Ad-hoc utilities.";
  license = stdenv.lib.licenses.mit;
}
