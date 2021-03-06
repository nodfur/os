{ pkgs, lib, config, ... }:

let
  jmk-x11-fonts =
    pkgs.stdenv.mkDerivation rec {
      pname = "jmk-x11-fonts";
      version = "1.0";
      name = "${pname}-${version}";

      src = pkgs.fetchFromGitHub {
        owner = "nikolas";
        repo = pname;
        rev = "29ae539d18005b9f9a863870cd181f208f08181b";
        sha256 = "0cih76ripzampfwg89yh96ajzlp9b4dzwkcxqxdw89z2g4wd1p46";
      };

      nativeBuildInputs = with pkgs.xorg; [mkfontdir mkfontscale];

      installPhase = ''
        mkdir -p $out/share/fonts/misc
        cp *.bdf $out/share/fonts/misc
        cd $out/share/fonts/misc
        mkfontdir && mkfontscale
      '';

      meta = with lib; {
        description = "Jim Knoble's fixed-width bitmap fonts";
        homepage = https://github.com/nikolas/jmk-x11-fonts;
        license = licenses.gpl2Plus;
        platforms = platforms.linux;
      };
    };

in {
  services.xserver.autoRepeatDelay = 200;
  services.xbanish.enable = true;

  services.xserver.displayManager.autoLogin.user = config.os.username;

   services.xserver.resolutions = [{ x = 3840; y = 2160; }];

  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps,compose:ralt";

  environment.systemPackages = with pkgs; [
    hsetroot
    xorg.xbacklight
  ];

  fonts.fonts = with pkgs; [
    dejavu_fonts
    fantasque-sans-mono
    emacs-all-the-icons-fonts
    # iosevka
    # google-fonts
    fira-code fira-code-symbols
    jmk-x11-fonts
    cm_unicode
    # roboto roboto-mono roboto-slab
    _3270font
  ];

  fonts.fontconfig.hinting.enable = false;

  services.redshift.enable = !config.os.vm && !config.os.wayland;

  programs.ssh.setXAuthLocation = !config.os.wayland;
  services.openssh.forwardX11 = !config.os.wayland;
}
