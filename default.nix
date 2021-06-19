{ haskellPackages }:
haskellPackages.callCabal2nix "deriving-trans" ./. {}
