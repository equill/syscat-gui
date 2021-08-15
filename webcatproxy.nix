{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
    "127.0.0.1" = [ "webcat.onfire.onice" ];
    "10.255.0.1" = [ "rgtest.onfire.onice" ];
  };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts = {
      "webcat" = {
        serverName = "localhost";
        serverAliases = [ "webcat"
                          "webcat.onfire.onice"
                          "syscat"
                          "syscat.onfire.onice"
                        ];
        listen = [ { addr = "127.0.0.1";
                     port = 80;
                     ssl = false;
                   }
                 ];
        locations = {
          # Dev ports
          #"/" = { proxyPass = "http://127.0.0.1:8080/"; };
          #"/schema/" = { proxyPass = "http://webcat.onfire.onice:4950/schema/"; };
          #"/raw/" = { proxyPass = "http://webcat.onfire.onice:4950/raw/"; };
          #"/files-api/" = { proxyPass = "http://webcat.onfire.onice:4950/files/"; };
          #
          # cl-webcat dev ports
          "/" = { proxyPass = "http://127.0.0.1:8080/"; };
          "/schema/" = { proxyPass = "http://webcat.onfire.onice:4965/schema/"; };
          "/raw/" = { proxyPass = "http://webcat.onfire.onice:4965/raw/"; };
          "/files-api/" = { proxyPass = "http://webcat.onfire.onice:4965/files/"; };
          #
          # Prod ports
          #"/" = { proxyPass = "http://webcat.onfire.onice:8080/"; };
          #"/schema/" = { proxyPass = "http://webcat.onfire.onice:4955/schema/"; };
          #"/raw/" = { proxyPass = "http://webcat.onfire.onice:4955/raw/"; };
          #"/files-api/" = { proxyPass = "http://webcat.onfire.onice:4955/files/"; };
        };
      };
    };
  };
}
