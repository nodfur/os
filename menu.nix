{ pkgs, lib, config, ... }:

let
  cmd = name: value: { inherit name value; };

  menu-commands = [
    (cmd "Rebuild System"
      ''
        os-terminal -e "sudo make -C /os || bash"
      '')

    (cmd "Restart Desktop"
      ''
        sudo systemctl restart display-manager
      '')

    (cmd "Emacs"
      ''emacs'')

    (cmd "Google Chrome"
      ''google-chrome-beta'')

    (cmd "Firefox"
      ''firefox'')

    (cmd "Telegram"
      ''${pkgs.tdesktop}/bin/telegram-desktop'')
  ];

  menu-names =
    pkgs.writeTextFile {
      name = "restless-menu-names";
      text =
        lib.concatMapStringsSep "\n"
          ({ name, ... }: name)
          menu-commands;
    };

  make-menu-script =
    i: { name, value }:
      pkgs.writeShellScriptBin "restless-menu-${toString i}" value;

  menu-scripts =
    pkgs.symlinkJoin {
      name =
        "restless-menu-scripts";
      paths =
        lib.imap1 make-menu-script menu-commands;
    };

  font-string = "${config.os.monospace.family} ${toString (config.os.monospace.size + 4)}";

  restless-menu =
    pkgs.writeShellScriptBin "restless-menu" ''
      i=$(${pkgs.rofi}/bin/rofi < ${menu-names} \
         -monitor -1 -dmenu -format d -i -p "${config.networking.hostName}" \
         -theme "${pkgs.rofi}/share/rofi/themes/gruvbox-light.rasi" \
         -theme-str '* { font: "${font-string}"; }'
     )
      exec ${menu-scripts}/bin/restless-menu-$i
    '';

in {
  environment.systemPackages = [restless-menu];
}
