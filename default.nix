let
  pkgs = import ./nixpkgs-pinned.nix;
in
pkgs.haskellPackages.developPackage {
  root = ./.;
}
