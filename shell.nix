# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

# { cabal, aeson, cairo, configurator, glib, gtk3, lens, mtl, time
# , transformers, vector, yap
# }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "whynote";
  version = "1";
  src = "./.";
  isLibrary = false;
  isExecutable = true;
  buildTools = [ cabalInstall ];
  buildDepends = [
    aeson cairo configurator glib gtk3 lens mtl time transformers
    vector yap unorderedContainers
  ];
  meta = {
    description = "A minimal note-taking program";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
