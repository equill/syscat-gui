{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
    "192.0.2.1" = [ "webcat-docker.onfire.onice" ];
    # webcat-test is for running stuff in the REPL instead of in containers.
    # To avoid conflicts with the default virtual server, it needs to run on a different lo addr.
    "127.0.0.2" = [ "webcat-test" "webcat-test.onfire.onice" ];
  };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts = {
      # Default local vserver
      localhost = {
        serverName = "localhost";
        # For running webcat-prod locally
        serverAliases = [ "webcat" "webcat.onfire.onice" ];
        #listen = [ { addr = "127.0.0.1"; port = 80; ssl = false; } ];
        locations = {
          # In prod, everything runs in a container.
          # On a development-only host, such as thing, these should fail to connect.
          "/" = { proxyPass = "http://webcat-docker.onfire.onice:8082/"; };
          "/schema/" = { proxyPass = "http://webcat-docker.onfire.onice:4965/schema/"; };
          "/raw/" = { proxyPass = "http://webcat-docker.onfire.onice:4965/raw/"; };
          "/files-api/" = { proxyPass = "http://webcat-docker.onfire.onice:4965/files/"; };
        };
      };
      # Vserver for running webcat-test locally
      "webcat-test" = {
        serverName = "webcat-test";
        # Listen on the _other_ loopback address.
        serverAliases = [ "webcat-test.onfire.onice" ];
        listen = [
          {
            addr = "webcat-test.onfire.onice";
            port = 80;
            ssl = false;
          }
        ];
        locations = {
          # Dev front-end listens on a loopback interface
          "/" = { proxyPass = "http://localhost:8083/"; };
          # Restagraph backend
          "/schema/" = { proxyPass = "http://webcat-docker.onfire.onice:4965/schema/"; };
          "/raw/" = { proxyPass = "http://webcat-docker.onfire.onice:4655/raw/"; };
          "/files-api/" = { proxyPass = "http://webcat-docker.onfire.onice:4965/files/"; };
        };
        extraConfig = ''
            rewrite ^/$ /display/Wikipages/Big_picture redirect;
        '';
      };
    };
  };
}
