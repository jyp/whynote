with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            ({ mkDerivation, aeson, base, bytestring, cairo, configurator
             , containers, glib, gtk3, lens, mtl, old-locale, stdenv, time
             , transformers, unordered-containers, vector, gasp
             }:
             mkDerivation {
               pname = "whynote";
               version = "1";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 aeson base bytestring cairo configurator containers glib gtk3 lens
                 mtl old-locale time transformers unordered-containers vector gasp
               ];
               description = "A minimal note-taking program";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
