{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
     "127.0.0.1" = [ "webcat.onfire.onice" ];
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
        locations."/schema/" = { proxyPass = "http://webcat.onfire.onice:4955/schema/"; };
        locations."/raw/" = { proxyPass = "http://webcat.onfire.onice:4955/raw/"; };
        locations."/files-api/" = { proxyPass = "http://webcat.onfire.onice:4955/files/"; };
        # Dev port:
        #locations."/" = { proxyPass = "http://127.0.0.1:8080/"; };
        # Docker port:
        locations."/" = { proxyPass = "http://webcat.onfire.onice:8080/"; };
      };
    };
  };
}
