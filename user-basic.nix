{
  programs.bash.enable = true;
  programs.bash.initExtra = ''
    . /nix/var/nix/profiles/per-user/$USER/home-manager/home-path/etc/profile.d/hm-session-vars.sh
  '';

  home.file.".config/i3/config".source = ./i3.config;
  home.file.".config/i3status/config".source = ./i3status.config;

  home.stateVersion = "22.05";

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
}
