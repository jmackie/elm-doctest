{ nixpkgs
, mkDerivation
, aeson
, ansi-terminal
, attoparsec
, base
, bytestring
, directory
, filepath
, process
, stdenv
, text
}:
mkDerivation {
    pname = "elm-doctest";
    version = "0.1.0";
    homepage = "https://github.com/jmackie/elm-doctest/";
    description = "Doctest runner for Elm";
    license = stdenv.lib.licenses.mit;
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = [
        aeson
        ansi-terminal
        attoparsec
        base
        bytestring
        directory
        filepath
        process
        stdenv
        text
    ];
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
        "-frelease"
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-L${nixpkgs.gmp6.override { withStatic = true; }}/lib"
        "--ghc-option=-optl=-L${nixpkgs.zlib.static}/lib"
        "--ghc-option=-optl=-L${nixpkgs.glibc.static}/lib"
    ];
}
