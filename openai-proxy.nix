{ ... }:

{
  services.nginx.virtualHosts."gpt3.node.town" = {
    forceSSL = true;
    useACMEHost = "node.town";
    locations = {
      "/" = {
        proxyPass = "http://localhost:9000/";
      };
    };
  };
}
