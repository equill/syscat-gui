with import <nixpkgs> {};

let
    syscatgui_deriv = stdenv.mkDerivation rec {
        name = "syscatgui";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils libressl;
        system = builtins.currentSystem;
        staticpath = ../../src/static;
        syscatguipath = ./syscatgui;
    };


    ld_path = pkgs.lib.makeLibraryPath [
        pkgs.libressl
    ];

    entrypoint = writeScript "entrypoint.sh" ''
    #!${stdenv.shell}
    export LD_LIBRARY_PATH=${ld_path}
    exec $@
    '';

in
pkgs.dockerTools.buildImage {
    name = "equill/syscatgui";
    tag = "0.0.1a1";
    created = "now";

    contents = [
        syscatgui_deriv
        bash
        coreutils
        glibc
        libressl
        which
    ];

    config = {
        Cmd = [ "syscatgui" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "8080/tcp" = {};
        };
        WorkingDir = "/";
    };
}
