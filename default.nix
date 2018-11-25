{ mkDerivation, base, containers, megaparsec, mtl, prettyprinter
, recursion-schemes, repline, stdenv, text, transformers
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
}
