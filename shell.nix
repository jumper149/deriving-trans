let
  nixpkgs = import ./nix/nixpkgs.nix;
  deriving-trans = import ./default.nix;
in
  nixpkgs.haskellPackages.shellFor {
    buildInputs = with nixpkgs.haskellPackages; [
      haskell-language-server
      hlint
      implicit-hie
    ];
    packages = haskellPackages: [
      (deriving-trans { inherit haskellPackages; })
    ];
    withHoogle = true;
  }

# TODO: workaround to use ghc-9.0.1
#  nixpkgs.mkShell {
#    inputsFrom = [
#      (nixpkgs.haskell.packages.ghc901.callCabal2nix "deriving-trans" ../. {}).env
#    ];
#  }
