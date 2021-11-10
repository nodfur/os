{ config, ... }:

{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.neomutt.enable = true;

  accounts.email = {
    accounts.nodetown = {
      primary = true;

      address = "${config.home.username}@node.town";
      userName = "${config.home.username}@node.town";
      realName = "${config.home.username}";

      passwordCommand = "cat /password";

      imap.host = "root.node.town";
      smtp.host = "root.node.town";

      msmtp.enable = true;

      mbsync.enable = true;
      mbsync.create = "maildir";

      neomutt.enable = true;
    };
  };
}
