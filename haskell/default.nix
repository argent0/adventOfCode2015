let
	pkgs = import (builtins.fetchGit {
		# Descriptive name to make the store path easier to identify
		name = "nixos-23.11.2596.c1be43e8e837";
		url = "https://github.com/nixos/nixpkgs/";
		# Commit hash for nixos-23.11 as of 2024-01-06
		# `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
		ref = "refs/heads/nixos-23.11";
		rev = "c1be43e8e837b8dbee2b3665a007e761680f0c3d";
	}) {};
in
pkgs.haskellPackages.developPackage {
	root = ./.;
}
