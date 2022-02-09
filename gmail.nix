{ lib, pkgs, config, ... }:

let
  cfg = config.os.gmail;

  gmail-params = pkgs.writeText "gmail-params.el" ''
    (setq
      user-mail-address "${cfg.address}"
      smtpmail-smtp-user "${cfg.address}"
      user-full-name "${cfg.name}"
      auth-source-pass-filename "~/.local/share/password-store")

    (require 'auth-source)
    (require 'auth-source-pass)
    (auth-source-pass-enable)

    (unless (car (auth-source-search :host "smtp.googlemail.com"))
      (user-error "Your pass entry for Gmail needs to be called `smtp.googlemail.com'."))

    (provide 'gmail-params)
  '';

  mbsyncrc = pkgs.writeText "mbsyncrc" ''
    IMAPAccount gmail
    Host imap.gmail.com
    User ${cfg.address}
    PassCmd "${cfg.passwordCommand}"
    AuthMechs LOGIN
    SSLType IMAPS
    CertificateFile ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    
    IMAPStore gmail-remote
    Account gmail
    
    MaildirStore gmail-local
    Subfolders Verbatim
    Path ~/Maildir/
    Inbox ~/Maildir/INBOX
    
    Channel gmail
    Master :gmail-remote:
    Slave :gmail-local:
    Patterns ${lib.concatMapStringsSep " " (x: "\"${x}\"") cfg.patterns}
    Create Both
    SyncState *
  '';

  gmail-mbsync = pkgs.writeShellScriptBin "gmail-mbsync" ''
    ${pkgs.isync}/bin/mbsync -c ${mbsyncrc} "$@"
  '';

  gmail-elisp =
    pkgs.stdenv.mkDerivation {
      name = "gmail-elisp-1.0";
      src = ./gmail.el;
      unpackPhase = "true";
      installPhase = ''
        dir=$out/share/emacs/site-lisp
        mkdir -p "$dir"
        install "$src" "$dir/gmail.el"
        install "${gmail-params}" "$dir/gmail-params.el"
      '';
    };

in {
  config = lib.mkIf cfg.enable {
    environment.systemPackages = [
      pkgs.gnutls
      pkgs.html2text
      pkgs.mu
      gmail-mbsync
      gmail-elisp
    ];
  };
}
