{ pkgs ? import (builtins.fetchTarball {
  name = "nixpkgs-23.11-2024-01-27";
  # git ls-remote https://github.com/nixos/nixpkgs nixos-23.11
  url = "https://github.com/nixos/nixpkgs/archive/a77ab169a83a4175169d78684ddd2e54486ac651.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0r9a87aqhqr7dkhfy5zrx2dgqq11ma2rfvkfwqhz1xqg7y6mcxxg";
}) { } }:
let
  vimrc = builtins.readFile ./vimrc;
  vimPlugins = {
    start = (with pkgs.vimPlugins; [
      vim-nix
      nvim-lspconfig
      copilot-vim
    ]);
    opt = (with pkgs.vimPlugins; [
    ]);
  };
  local-neovim = pkgs.neovim.override {
    configure = {
      customRC = vimrc;
      packages.myVimPackage = vimPlugins;
    };
  };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    local-neovim
    ghc
    ( haskell-language-server.override { supportedGhcVersions = [ "94" ]; } )
    ghcid
    cabal-install
    haskellPackages.doctest
  ];
}
