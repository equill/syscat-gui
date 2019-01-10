with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "cl-webcat";

    buildInputs = [
        # Neo4j
        pkgs.neo4j
        # Lisp env
        pkgs.sbcl
    ];

    env = buildEnv { name = name; paths = buildInputs; };

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix cl-webcat] \\w\\$\\[\\033[00m\\] '";

}
