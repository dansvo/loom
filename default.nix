{ mkDerivation, base, diagrams, diagrams-lib, diagrams-svg
, palette, random, MonadRandom, QuickCheck, mtl, filepath, stdenv, lib
}:
mkDerivation {
  pname = "loom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    diagrams
    diagrams-lib
    diagrams-svg
    palette
    random
    MonadRandom
    QuickCheck
    mtl
    filepath
  ];
  homepage = "https://github.com/dansvo/loom#readme";
  license = lib.licenses.bsd3;
}
