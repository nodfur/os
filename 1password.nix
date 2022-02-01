{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    _1password
    _1password-gui
  ];

  users.groups.onepassword.gid = 44399;

  security.wrappers = {
    "1Password-BrowserSupport" = {
      source =
        "${pkgs._1password-gui}/share/1password/1Password-BrowserSupport";
      owner = "root";
      group = "onepassword";
      setuid = false;
      setgid = true;
    };

    "1Password-KeyringHelper" = {
      source =
        "${pkgs._1password-gui}/share/1password/1Password-KeyringHelper";
      owner = "root";
      group = "onepassword";
      setuid = true;
      setgid = true;
    };
  };
}
