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
        };
        cabalOptions =
          let
            flags = import ./flags.nix;
            setFlag = name: on: "${if on then "-f+" else "-f-"}${name}";
          in
            lib.strings.escapeShellArgs (builtins.attrValues (builtins.mapAttrs setFlag flags));

      in (haskell.packages.ghc96.extend overlay).callCabal2nixWithOptions "deriving-trans" source cabalOptions {};

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      haskell.packages.ghc96.shellFor {
        buildInputs = with haskell.packages.ghc96; [
          cabal-install
#          ghcid
          haskell-language-server
#          hlint
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.default
        ];
        withHoogle = false;
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
