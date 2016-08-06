{ mkDerivation, aeson, base, bytestring, cairo, configurator
, containers, gasp, glib, gtk3, lens, mtl, old-locale, QuickCheck
, stdenv, time, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "whynote";
  version = "1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring cairo configurator containers gasp glib gtk3
    lens mtl old-locale QuickCheck time transformers
    unordered-containers vector
  ];
  description = "A minimal note-taking program";
  license = stdenv.lib.licenses.gpl3;
}
