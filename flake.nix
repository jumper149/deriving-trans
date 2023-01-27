{
  description = "Derive instances for monad transformer stacks";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
    monad-control-identity = {
      type = "github";
      owner = "jumper149";
      repo = "monad-control-identity";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, monad-control-identity }: {

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        src = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
          base-orphans = pkgs.haskell.lib.dontCheck super.base-orphans;
          exceptions = super.exceptions_0_10_7;
          monad-control-identity = self.callCabal2nix "monad-control-identity" monad-control-identity.outPath {};
          mtl = super.mtl_2_3_1;
          resourcet = pkgs.haskell.lib.dontCheck super.resourcet;
          transformers = super.transformers_0_6_0_4;
        };
      in (haskellPackages.extend overlay).callCabal2nix "deriving-trans" src {};

    packages.x86_64_linux.devShell =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          ghcid
          haskell-language-server
          hlint
          hnix
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.defaultPackage.x86_64-linux
        ];
        withHoogle = true;
      };

  };
}
