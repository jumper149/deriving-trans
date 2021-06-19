let
  nixpkgs = import ./nix/nixpkgs.nix;
in
  nixpkgs.callPackage (import ./default.nix) {}
