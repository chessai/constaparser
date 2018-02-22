{ package ? "constaparser", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).constaparser
