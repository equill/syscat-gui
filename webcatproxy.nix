{ config, pkgs, ...}:

{
  environment.systemPackages = [
    pkgs.nginx
  ];

  networking.hosts = {
     "127.0.0.1" = [ "narcisse.onfire.onice" ];
   };

  services.nginx = {
    enable = true;
    statusPage = true;

    virtualHosts = {
      "narcisse" = {
        serverName = "localhost";
        serverAliases = [ "narcisse" "narcisse.onfire.onice" ];
        listen = [ { addr = "127.0.0.1";
                     port = 80;
                     ssl = false;
                   }
                 ];
        locations."/schema/" = { proxyPass = "http://10.255.0.1:4955/schema/"; };
        locations."/raw/" = { proxyPass = "http://10.255.0.1:4955/raw/"; };
        locations."/" = { proxyPass = "http://127.0.0.1:8080/"; };
      };
    };
  };
}
