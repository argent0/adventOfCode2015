{
  description = "A very basic flake";

  inputs = {
  	flake-nvim-haskell.url = "github:argent0/flake-nvim-haskell";
  };

  outputs = { self, nixpkgs, flake-nvim-haskell}: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {

    packages.x86_64-linux.default = flake-nvim-haskell.lib.neovimForHaskell { 
    	extraNixDerivations = with pkgs; [
          ghc
          ( haskell-language-server.override { supportedGhcVersions = [ "94" ]; } )
          ghcid
          cabal-install
	];
    };

  };
}
