with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "syscat-gui";

    buildInputs = [
        pkgs.sbcl
        # SSL
        pkgs.libressl
    ];

    env = buildEnv { name = name; paths = buildInputs; };

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
        pkgs.libressl
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix syscat-gui] \\w\\$\\[\\033[00m\\] '";
}
