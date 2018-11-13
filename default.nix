let
    pkgs =
        import <nixpkgs> {};

    project =
        { mkDerivation
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
            version = "0.1.0.0";
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

            # Static linking
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-pthread"
                "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
                "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
                "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
            ];
        };
in
pkgs.haskellPackages.callPackage project {}
