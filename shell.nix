with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            (./default.nix) {};
in
  stdenv.mkDerivation {
  name = "whynote-haskell-env";
  buildInputs = pkg.buildInputs ++ [ haskellPackages.intero haskellPackages.cabal-install];
}
