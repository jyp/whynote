with (import <nixpkgs> {}).pkgs;
let pkg = haskellPackages.callPackage
            (./default.nix) {};
in
  pkg.env
