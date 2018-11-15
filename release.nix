{ compiler ? "ghc844" }:
let
    nixpkgs =
        import <nixpkgs> {};
in
nixpkgs.haskell.packages.${compiler}.callPackage ./default.nix { inherit nixpkgs; }
