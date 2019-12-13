{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "argo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Zero-dep JSON parser with Haskell and Nix";
  license = stdenv.lib.licenses.mit;
}
