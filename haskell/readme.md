# haskellPackages.developPackage

1. Create the cabal project.

```
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ cabal-install ])" --run "cabal init"
```

2. Create default.nix

```
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
```

3. Build

```
$ nix-build
$ ./result/bin/yourprogram
```

4. Shell

```
nix-shell
```


