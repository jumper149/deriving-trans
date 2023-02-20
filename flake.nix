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
        source = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
          base-orphans = pkgs.haskell.lib.dontCheck super.base-orphans;
          exceptions = super.exceptions_0_10_7;
          logict = pkgs.haskell.lib.dontCheck super.logict;
          monad-control-identity = self.callCabal2nix "monad-control-identity" monad-control-identity.outPath {};
          mtl = super.mtl_2_3_1;
          resourcet = pkgs.haskell.lib.dontCheck super.resourcet;
          transformers = super.transformers_0_6_1_0;
        };
        cabalOptions =
          let
            flags = import ./flags.nix;
            setFlag = name: on: "${if on then "-f+" else "-f-"}${name}";
          in
            lib.strings.escapeShellArgs (builtins.attrValues (builtins.mapAttrs setFlag flags));

      in (haskellPackages.extend overlay).callCabal2nixWithOptions "deriving-trans" source cabalOptions {};

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          ghcid
          haskell-language-server
          hlint
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.default
        ];
        withHoogle = true;
      };

    checks.x86_64-linux.build = self.packages.x86_64-linux.default;

    checks.x86_64-linux.shell = self.devShells.x86_64-linux.default;

    checks.x86_64-linux.warning =
      with import nixpkgs { system = "x86_64-linux"; };
      let override = old: {
        configureFlags = [
          "--ghc-option=-Werror"
        ];
      };
      in haskell.lib.overrideCabal self.packages.x86_64-linux.default override;

  };
}
