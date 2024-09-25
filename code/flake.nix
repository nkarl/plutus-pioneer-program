{
  description = "A Nix flake for a Haskell development environment.";

  inputs =
  {
    #haskellNix.url = "github:input-output-hk/haskell.nix";
    #nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = { self, nixpkgs, ... }:
  let
    name = "plutus-pioneer-program";
    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    pkgs = import nixpkgs { };
  in
  {
    devShell = forAllSystems (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      pkgs.mkShell {
        inherit name;
        buildInputs =
          [
            pkgs.haskell.compiler.ghc810
            pkgs.haskellPackages.haskell-language-server
            pkgs.zlib
          ];

          shellHook = ''
          export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
          export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
        '';

        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      }
    );
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
  };
}
