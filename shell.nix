with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "cl-webcat";

    buildInputs = [
        # Neo4j
        #pkgs.neo4j
        # Lisp env
        pkgs.sbcl
        # SSL
        pkgs.libressl
    ];

    env = buildEnv { name = name; paths = buildInputs; };

    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
        pkgs.libressl
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix cl-webcat] \\w\\$\\[\\033[00m\\] '";
}
