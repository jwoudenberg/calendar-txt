{
  description = "calendar-txt";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        calendar-txt =
          pkgs.haskellPackages.callCabal2nix "calendar-txt" ./. { };
      in {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables calendar-txt;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ calendar-txt ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.ormolu
          ];
        };
      });
}
