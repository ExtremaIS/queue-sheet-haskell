{ mkDerivation, aeson, ansi-wl-pprint, base, directory, filepath
, ginger, optparse-applicative, process, scientific, stdenv, text
, transformers, ttc, yaml
}:
mkDerivation {
  pname = "queue-sheet";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-wl-pprint base directory filepath ginger
    optparse-applicative process scientific text transformers ttc yaml
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ExtremaIS/queue-sheet-haskell#readme";
  description = "queue sheet utility";
  license = stdenv.lib.licenses.mit;
}
