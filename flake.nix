{
  inputs = {

    nixpkgs.url =
      github:nodfur/nixpkgs/nodfur;

    flake-utils.url =
      github:numtide/flake-utils;

    home-manager.url =
      github:nix-community/home-manager;

    emacs-overlay.url =
      github:nix-community/emacs-overlay;

    mailserver.url =
      gitlab:simple-nixos-mailserver/nixos-mailserver/master;

    urbit-src = {
      flake = false;
      url = github:nodfur/urbit;
    };

    figlet-fonts = {
      flake = false;
      url = github:xero/figlet-fonts;
    };

    mu4e-dashboard = {
      flake = false;
      url = github:rougier/mu4e-dashboard;
    };

    mu4e-thread-folding = {
      flake = false;
      url = github:rougier/mu4e-thread-folding;
    };

    nano-emacs = {
      flake = false;
      url = github:rougier/nano-emacs;
    };

    urbit-emacs = {
      flake = false;
      url = github:clonex10100/urbit-api.el;
    };

    picom-src = {
      flake = false;
      url = github:ibhagwan/picom;
   };

    bcm2835-src = {
      flake = false;
      url = http://www.airspayce.com/mikem/bcm2835/bcm2835-1.70.tar.gz;
    };

    waveshare-epaper-demo-src = {
      flake = false;
      url = github:waveshare/IT8951-ePaper;
    };

    papertty-src = {
      flake = false;
      url = github:joukos/PaperTTY;
    };

    nodfur-it8951-src = {
      flake = false;
      url = github:nodfur/nodfur-it8951;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    home-manager,
    emacs-overlay,
    figlet-fonts,
    urbit-src,
    mailserver,
    nano-emacs,
    mu4e-dashboard,
    mu4e-thread-folding,
    urbit-emacs,
    picom-src,
    bcm2835-src,
    waveshare-epaper-demo-src,
    papertty-src,
    nodfur-it8951-src
  }@inputs:

    let
      accounts = import ./accounts.nix;

      common-modules = [
        overlays-module
        home-manager.nixosModules.home-manager
        urbit-module
        nixReplModule

        (import ./common.nix)
        (import ./pkgs.nix)
      ];

      overlays-module = {
        nixpkgs.overlays = all-overlays;
      };

      all-overlays = [
        emacs-overlay.overlay
        figlet-fonts-overlay
        nano-emacs-overlay
        openai-overlay
        picom-overlay
        telegram-overlay
        urbit-emacs-overlay
        urbit-overlay
      ];

      figlet-fonts-overlay =
        _self: _super: {
          inherit (inputs) figlet-fonts;
        };

      emacs-pkgs = pkgs: epkgs: with epkgs; [
        ag
        company
        company-nixos-options
        default-text-scale
        elixir-mode
        lispy
        lsp-mode
        lsp-ui
        magit
        nix-mode
        paredit
        pdf-tools
        projectile
        rainbow-delimiters
        selectrum
        selectrum-prescient
        slime
        vterm
        which-key
        whitespace-cleanup-mode
        zenburn-theme
        zig-mode

        pkgs.urbit-emacs
        pkgs.nano-emacs
      ];

      openai-overlay =
        import ./openai-overlay.nix;

      telegram-overlay =
        import ./telegram-overlay.nix;

      nano-emacs-overlay =
        import ./nano-emacs-overlay.nix inputs;

      picom-overlay =
        self: super: {
          picom = super.picom.overrideAttrs (_: {
            src = picom-src;
          });
        };

      urbit-overlay =
        self: super: (
          import "${urbit-src}" {
            inherit (super.stdenv.hostPlatform) system;
          }
        );

      urbit-emacs-overlay =
        self: super: {
          urbit-emacs =
            self.emacsPackages.trivialBuild {
              pname = "urbit-emacs";
              version = "0.0";
              src = inputs.urbit-emacs;
              packageRequires = with self.emacsPackages; [
                aio request
              ];
            };
        };

      bcm2835-overlay =
        self: super: {
          bcm2835 = super.stdenv.mkDerivation {
            name = "bcm2835";
            src = bcm2835-src;
          };

          waveshare-epaper-demo = super.stdenv.mkDerivation {
            name = "waveshare-epaper-demo";
            src = "${waveshare-epaper-demo-src}/Raspberry";
            buildInputs = [self.bcm2835];

            installPhase = ''
              mkdir -p $out/bin
              cp epd $out/bin/waveshare-epd
            '';
          };

          nodfur-it8951 = super.mkYarnPackage {
            name = "nodfur-it8951";
            src = nodfur-it8951-src;
          };
        };

      urbit-module =
        { config, lib, ... }: {
          environment.systemPackages =
            let
              urbit = import "${urbit-src}" {
                inherit (config.nixpkgs) system;
              };
            in
              [urbit.urbit];
        };

      firecrackerSystem1 = { isContainer }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            (
              import ./firecracker-base-system.nix {
                inherit nixpkgs;
                isContainer = isContainer;
              }
            )
          ];
        };

      nixReplModule = { pkgs, ... }: {
        nix.nixPath =
          let
            path = toString ./.;
          in [
            "repl=${path}/repl.nix"
            "nixpkgs=${nixpkgs}"
          ];

        environment.systemPackages = [
          (pkgs.writeShellScriptBin "os-nix-repl" ''
            source /etc/set-environment
            id=$(echo $NIX_PATH | perl -pe 's|.*(/nix/store/.*-source/repl.nix).*|\1|')
            nix repl "$id"
          '')
        ];
      };

      accountModule = { number, username, sshKeys }:
        { pkgs, ... }: {
          mailserver.loginAccounts."${username}@node.town" = {
            hashedPasswordFile = "/vm/by-name/${username}/password.hash";
          };

          system.activationScripts = {
            "vm-${username}-setup" = ''
              set -e
              echo "setting up vm${toString number} (${username})..."
              path=/vm/by-id/${toString number}
              mkdir -p $path
              cd $path
              if [ ! -f password.orig ]; then
                password=$(${pkgs.apg}/bin/apg -M L -n 1 -m 9 -x 9)
                echo "$password" >password.orig
                ${pkgs.apacheHttpd}/bin/htpasswd \
                  -nbB "" "$password" | cut -d: -f2 >password.hash
              fi

              mkdir -p /vm/by-name
              ln -sf $path /vm/by-name/${username}
            '';
          };
        };

      firecrackerSystem2 = {
        number, username, sshKeys
      }:
        nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = common-modules ++ [
            (import ./vm-lifestyle.nix {
              inherit nixpkgs;
              inherit number username sshKeys;

              keys = import ./keys.nix;
              ports = import ./ports.nix;
              domain = "node.town";
            })
          ];
        };

      systems = {
        chapel = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = common-modules ++ [
            mailserver.nixosModule

            (import ./b14.nix)

            ({ pkgs, ... }: {
              nixpkgs.overlays = [bcm2835-overlay];
              environment.systemPackages = with pkgs; [
                bcm2835
                waveshare-epaper-demo
              ];
            })

            (import ./firecracker-guests.nix { inherit self; })

            {
              mailserver = {
                enable = true;
                fqdn = "mail.node.town";
                sendingFqdn = "node.town";
                domains = ["node.town"];

                certificateScheme = 1;
                certificateFile = "/var/lib/acme/node.town/cert.pem";
                keyFile = "/var/lib/acme/node.town/key.pem";

                fullTextSearch = {
                  enable = true;
                  autoIndex = true;
                  indexAttachments = true;
                  enforced = "body";
                };

                localDnsResolver = false;

                indexDir = "/var/lib/dovecot/indices";
              };
            }

            {
              imports = builtins.map accountModule accounts;
            }

            {
              restless.firecracker.instances =
                builtins.map
                  ({ number, username, ... }: {
                    inherit number;
                    tapName = "tap${toString number}";
                    hostname = username;
                    localHostname = "${username}.local";
                    ip = "172.16.${toString number}.2";
                  })
                  (import ./accounts.nix);
            }

            ({ pkgs, ... }: {
              users.users =
                let
                  welcome = pkgs.writeShellScript "welcome" ''
                    catlet() {
                      ${pkgs.figlet}/bin/figlet -f ${pkgs.figlet-fonts}/$1.flf "$2" \
                        | ${pkgs.lolcat}/bin/lolcat
                    }

                    echo
                    catlet Bloody node.town
                    echo

                    echo "Welcome, $(tput bold)$(whoami)$(tput sgr0), to the $(tput dim)node.town$(tput sgr0) cluster."
                    echo

                    tmp=$(mktemp -d)
                    id="$tmp"/id_ed25519
                    cp "${./id_ed25519}" "$id"
                    chmod 600 "$id"

                    export NIX_SSHOPTS="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=QUIET -i $id"

                    set -e
                    ssh $NIX_SSHOPTS admin@$(whoami).local os-tailscale
                  '';

                  mkSshThing = key: ''command="${welcome}",no-port-forwarding,no-x11-forwarding,no-agent-forwarding ${key}'';

                  mkUser = keys: {
                    isNormalUser = true;
                    openssh.authorizedKeys.keys = builtins.map mkSshThing keys;
                  };

                  keys = import ./keys.nix;

                in {
                  lucy = mkUser keys.mbrock;
                  dbrock = mkUser keys.dbrock;
                  rainbreak = mkUser keys.rainbreak;
                };
            })

            ({ pkgs, ... }: {
              environment.systemPackages = [
                (pkgs.runCommand "copy-files" {} ''
                 mkdir -p $out/var/lib/rig
                 ln -s "${self.firecracker-vmlinux}/vmlinux" $out/var/lib/rig/vmlinux
                 ln -s "${self.firecracker-rootfs-qemu}/nixos.img" $out/var/lib/rig/rootfs
                '')
             ];
           })
          ];
        };

        armory = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = common-modules ++ [
            (import ./armory.nix)
          ];
        };

        urbion = nixpkgs.lib.makeOverridable nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = common-modules ++ [
            (import ./pi.nix)
            (import ./urbion.nix)

            {
              nixpkgs.config.allowUnfree = true;
            }

            ({ pkgs, ... }: {
              nixpkgs.overlays = [bcm2835-overlay];
              environment.systemPackages = with pkgs; [
                bcm2835
                waveshare-epaper-demo
                nodfur-it8951
              ];
            })

            ({ modulesPath, ... }: {
              imports = ["${modulesPath}/installer/sd-card/sd-image-aarch64.nix"];
            })
          ];
        };

        hetzner = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = common-modules ++ [
            (import ./hetzner-system.nix {
              inherit self nixpkgs;
            })

            ./users/mbrock

            # (import ./firecracker-guests.nix {
            #   inherit self;
            # })
            #
            # {
            #   restless.firecracker.instances = [];
            # }
          ];
        };
      };

    in {
      nixosConfigurations =
        systems // (
          builtins.listToAttrs (
            builtins.map
              ({ number, username, sshKeys }: {
                name = "vm${toString number}";
                value = firecrackerSystem2 {
                  inherit number username sshKeys;
                };
              })
              (import ./accounts.nix)
          )
        );

      firecracker-vmlinux = (
        firecrackerSystem1 { isContainer = false; }
      ).config.system.build.kernel.dev;

      firecracker-rootfs-qemu = (
        firecrackerSystem1 { isContainer = true; }
      ).config.system.build.rootfs-qemu;

      urbion-img =
        systems.urbion.config.system.build.sdImage;
    } // (
      flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = all-overlays;
          };
        in rec {
          packages = {
            inherit (pkgs) waveshare-epaper-demo;

            nodfur-emacs =
              let
                emacs = pkgs.emacsWithPackages (emacs-pkgs pkgs);
              in pkgs.writeShellScriptBin "nodfur-emacs" ''
                ${emacs}/bin/emacs --load ${./emacs-init.el}
              '';

            nodfur-emacs-packages =
              pkgs.emacsPackages;
          };
        }
      )
    );
}
