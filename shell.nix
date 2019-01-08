with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "cl-webcat";

    buildInputs = [
        # Neo4j
        pkgs.neo4j
        # Lisp env
        #pkgs.gcc_multi
        #pkgs.gcc
        #pkgs.libyaml
        #pkgs.openssl
        pkgs.sbcl
        # Python env
        #pkgs.python36Packages.requests
        #pkgs.python36Packages.pylint
        #pkgs.python3
    ];

    env = buildEnv { name = name; paths = buildInputs;
    };

    #LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
    #    pkgs.openssl
    #    pkgs.libyaml
    #];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix cl-webcat] \\w\\$\\[\\033[00m\\] '";

}
