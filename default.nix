{ pkgs ? import <nixpkgs> {} }:
let
  hakyll-agda = pkgs.haskellPackages.callCabal2nix
    "hakyll-agda"
    (builtins.filterSource
      (path: type:
        builtins.elem (baseNameOf path) ["agda.css" "Agda.hs" "hakyll-agda.cabal" "LICENSE" "Setup.hs"])
      ./.)
    {};
  node = import ./node.nix { inherit pkgs; };
in {
  inherit hakyll-agda;
}
