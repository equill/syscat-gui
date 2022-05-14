with import <nixpkgs> {};

let
    webcatgui_deriv = stdenv.mkDerivation rec {
        name = "webcatgui";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils libressl;
        system = builtins.currentSystem;
        staticpath = ../../src/static;
        webcatguipath = ./webcatgui;
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
    name = "equill/webcatgui";
    tag = "0.0.12a1";
    created = "now";

    contents = [
        webcatgui_deriv
        bash
        coreutils
        glibc
        libressl
        which
    ];

    config = {
        Cmd = [ "webcatgui" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "8080/tcp" = {};
        };
        WorkingDir = "/";
    };
}
