{
	description = "A very basic flake";

	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = nixpkgs.legacyPackages.${system};
				lib = nixpkgs.lib;
			in rec {
				packages.default = pkgs.stdenv.mkDerivation {
					pname = "logic-edit";
					version = "0.0.1";
					src = ./.;

					nativeBuildInputs = with pkgs; [
						ghc
						haskellPackages.BNFC
						haskellPackages.alex
						haskellPackages.happy
					];

					installPhase = ''
						mkdir -p $out/bin
						cp Main-opt $out/bin/logic-edit
					'';
				};

				devShells.default = pkgs.mkShell {
					nativeBuildInputs = with pkgs; [
						haskellPackages.haskell-language-server
						haskellPackages.ghc
						haskellPackages.stylish-haskell
						haskellPackages.hindent
						haskellPackages.BNFC
						haskellPackages.alex
						haskellPackages.happy
						idris2
						gnumake

						cargo
						rustc
						rustfmt
						clippy
						rust-analyzer
						cargo-expand
					];
				};
			}
		);
}
