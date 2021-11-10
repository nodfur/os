{ pkgs, ... }:

let ports = import ./ports.nix;

in {
  services.nginx.virtualHosts."dash.node.town" = {
    forceSSL = true;
    useACMEHost = "node.town";
    locations = {
      "/" = {
        proxyPass = "http://localhost:${toString ports.grafana}/";
      };
    };
  };

  services.grafana = {
    enable = true;
    port = ports.grafana;
    domain = "dash.node.town";
    rootUrl = "https://dash.node.town/";
  };

  services.influxdb2 = {
    enable = true;
    settings = {
      "http-bind-address" = ":${toString ports.influxdb}";
    };
  };

  services.telegraf = {
    enable = true;
    extraConfig = {
      outputs.influxdb = {
        urls = ["http://localhost:${toString ports.influxdb}"];
        database = "telegraf";
      };

      agent.interval = "1s";
      agent.flush_interval = "1s";

      inputs.cpu = {
        percpu = true;
        totalcpu = true;
      };

      inputs.disk = {
        ignore_fs = ["tmpfs" "devtmpfs"];
      };

      inputs.kernel = {};
      inputs.mem = {};
      inputs.net = {};
      inputs.processes = {};
    };
  };

  services.grafana.provision = {
    enable = true;
    datasources = [
      {
        name = "influxdb";
        type = "influxdb";
        access = "proxy";
        url = "http://localhost:${toString ports.influxdb}";
        isDefault = true;
      }
    ];
  };
}
