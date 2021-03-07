with import <nixpkgs> {};

let
    clwebcat_deriv = stdenv.mkDerivation rec {
        name = "clwebcat";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils libressl;
        system = builtins.currentSystem;
        templatepath = ../../src/templates;
        staticpath = ../../src/static;
        clwebcatpath = ./clwebcat;
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
    name = "equill/clwebcat";
    tag = "0.0.6";
    created = "now";

    contents = [
        clwebcat_deriv
        bash
        coreutils
        glibc
        libressl
        which
    ];

    config = {
        Cmd = [ "clwebcat" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "8080/tcp" = {};
        };
        WorkingDir = "/";
    };
}
