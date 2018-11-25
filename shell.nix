{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, megaparsec, mtl
      , prettyprinter, recursion-schemes, repline, stdenv, text
      , transformers
      }:
      mkDerivation {
        pname = "abstract-machine-debuggers";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base containers megaparsec mtl prettyprinter recursion-schemes
          repline text transformers
        ];
        description = "Debuggers for abstract machines";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
