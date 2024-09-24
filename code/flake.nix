{
  description = "A Nix flake for a Haskell development environment.";

  inputs =
  {
    #haskellNix.url = "github:input-output-hk/haskell.nix";
    #nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = { self, nixpkgs, ... }:
  let
    name = "plutus-pioneer-program";
    supportedSystems = [ "x86_64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
  in
  {
    devShell = forAllSystems (
      system:
      let
        #pkgs = import nixpkgs { inherit system; };
        pkgs = import nixpkgs { inherit system; } //
        {
          #ghc = import (builtins.fetchGit {
            #name = "ghc-8.10.7";
            #url = "https://github.com/NixOS/nixpkgs/";
            #ref = "refs/heads/nixos-23.05";
            ##rev = "214e2d6bee035e468f9d420be19b6b6d9aa9b027";
            #rev = "ed03b7af334826159b6e8a1373858f2844ba8df6";
          #}) {};
          #hls = import (builtins.fetchGit {
            #name = "hls-1.10.0.0";
            #url = "https://github.com/NixOS/nixpkgs/";
            #ref = "refs/heads/nixos-23.05";
            #rev = "ed03b7af334826159b6e8a1373858f2844ba8df6";
          #}) {};
          #secp256k1 = import (builtins.fetchGit {
            #name = "secp256k1";
            #url = "https://github.com/NixOS/nixpkgs/";
            #ref = "refs/heads/nixos-23.05";
            #rev = "ed03b7af334826159b6e8a1373858f2844ba8df6";
          #}) {};
          #libsodium = import (builtins.fetchGit {
            #name = "libsodium";
            #url = "https://github.com/NixOS/nixpkgs/";
            #ref = "refs/heads/nixos-23.05";
            #rev = "ed03b7af334826159b6e8a1373858f2844ba8df6";
          #}) {};
        };
      in
      pkgs.mkShell {
        inherit name;
        buildInputs =
          [
            pkgs.haskell.compiler.ghc96 # 810
            pkgs.haskellPackages.haskell-language-server
            #pkgs.ghc
            #pkgs.haskell-language-server
            #pkgs.sbclPackages.secp256k1
            #pkgs.haskellPackages.libsodium
            pkgs.secp256k1
            pkgs.libsodium
          ];
      }
    );
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
  };
}

#{
#description = "A Nix flake for Haskell development environment";

#inputs = {
#nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
#};

#outputs = { self, nixpkgs, ... }: let

#system = "x86_64-linux";

#in {
#devShells."${system}".default = let
#pkgs = import nixpkgs {
#inherit system;
#};

#in pkgs.mkShell {
#packages = with pkgs; [
#cabal-install
#stack
#haskell.compiler.ghc8107
#haskell-language-server
#];

#shellHook = ''
#echo "ghc `${pkgs.haskell.compiler.ghc8107}/bin/ghc --version`"
#'';
#};
#};
#}
