let
  commit = "1f91fd1040667e9265a760b0347f8bc416249da7";

  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "1lcfcwgal9fpaiq71981abyzz160r6nx1y4pyy1dnvaf951xkdcj";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  }
