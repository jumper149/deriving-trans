self: super: {
  haskellPackages = super.haskell.packages.ghc8104.override {
    overrides = selfHaskellPackages: superHaskellPackages: {
    };
  };
}
