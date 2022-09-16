let
    pkgs = import <nixpkgs> { };
    star-data = import ./star-data.nix {};
    stars = star-data.hyg-star-data;
    lines = star-data.constellation-lines;
in
  rec {
    program = pkgs.haskellPackages.callPackage ./default.nix { };
    chart = pkgs.stdenv.mkDerivation {
      name = "rug.svg";
      program = program;
      buildCommand = ''
        declare -xp
        mkdir $out
        $program/bin/frac-tiles $out
      '';
    };
  }
