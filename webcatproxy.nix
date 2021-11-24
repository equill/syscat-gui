{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
    #"127.0.0.2" = [ "webcat-ng.onfire.onice" ];
    "192.0.2.1" = [ "rgtest.onfire.onice" "webcat-rg.onfire.onice" ];
  };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts = {
      localhost = {
        serverName = "localhost";
        serverAliases = [ "webcat"
                          "webcat.onfire.onice"
                          "syscat"
                          "syscat.onfire.onice"
                        ];
        #listen = [ { addr = "127.0.0.1"; port = 80; ssl = false; } ];
        locations = {
          # Dev ports
          #"/" = { proxyPass = "http://127.0.0.1:8080/"; };
          #"/schema/" = { proxyPass = "http://webcat.onfire.onice:4950/schema/"; };
          #"/raw/" = { proxyPass = "http://webcat.onfire.onice:4950/raw/"; };
          #"/files-api/" = { proxyPass = "http://webcat.onfire.onice:4950/files/"; };
          #
          # cl-webcat dev ports
          "/" = { proxyPass = "http://webcat-rg.onfire.onice:8080/"; };
          "/schema/" = { proxyPass = "http://webcat.onfire.onice:4955/schema/"; };
          "/raw/" = { proxyPass = "http://webcat.onfire.onice:4955/raw/"; };
          "/files-api/" = { proxyPass = "http://webcat.onfire.onice:4955/files/"; };
          #
          # Prod ports
          #"/" = { proxyPass = "http://webcat.onfire.onice:8080/"; };
          #"/schema/" = { proxyPass = "http://webcat.onfire.onice:4955/schema/"; };
          #"/raw/" = { proxyPass = "http://webcat.onfire.onice:4955/raw/"; };
          #"/files-api/" = { proxyPass = "http://webcat.onfire.onice:4955/files/"; };
        };
      };
      "webcat-ng" = {
        serverName = "webcat-ng";
        serverAliases = [ "webcat-ng.onfire.onice" "webcat.onfire.onice" ];
        listen = [
          {
            addr = "webcat-ng.onfire.onice";
            port = 80;
            ssl = false;
          }
        ];
        locations = {
          # Dev front-end listens on a loopback interface
          "/" = { proxyPass = "http://webcat-rg.onfire.onice:8082/"; };
          #"/" = { proxyPass = "http://webcat-ng.onfire.onice:8083/"; };
          # Restagraph backend
          "/schema/" = { proxyPass = "http://webcat-rg.onfire.onice:4965/schema/"; };
          "/raw/" = { proxyPass = "http://webcat-rg.onfire.onice:4655/raw/"; };
          "/files-api/" = { proxyPass = "http://webcat-rg.onfire.onice:4965/files/"; };
        };
      };
    };
  };
}
