{
  pinnedNixpkgsCommit ? "068984c00e0d4e54b6684d98f6ac47c92dcb642e", # nixos-20.09
  pinnedNixpkgsUrl ? "https://github.com/NixOS/nixpkgs/archive/${pinnedNixpkgsCommit}.tar.gz",
  pkgs ? import (fetchTarball pinnedNixpkgsUrl) {},
}:
let
  hakyll-agda = pkgs.haskellPackages.callCabal2nix
    "hakyll-agda"
    ./.
    {};
in {
  inherit hakyll-agda;
}
