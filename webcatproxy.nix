{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
    "127.0.0.1" = [ "narcisse" ];
  };
  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts = {
      "narcisse" = {
        serverName = "localhost";
        listen = [ { addr = "127.0.0.1";
                     port = 80;
                     ssl = false;
                   }
                 ];
        locations."/schema/" = { proxyPass = "http://10.255.0.1:4955/schema/"; };
        locations."/raw/" = { proxyPass = "http://10.255.0.1:4955/raw/"; };
        locations."/" = { proxyPass = "http://localhost:8080/"; };
      };
    };
  };
}
