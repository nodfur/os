let keys = import ./keys.nix;
in [
  {
    number = 1;
    username = "mbrock";
    sshKeys = keys.mbrock;
  }

  {
    number = 2;
    username = "lucy";
    sshKeys = [];
  }

  {
    number = 3;
    username = "dbrock";
    sshKeys = keys.dbrock;
  }

  {
    number = 4;
    username = "rainbreak";
    sshKeys = keys.rainbreak;
  }

  {
    number = 5;
    username = "drmaciver";
    sshKeys = keys.drmaciver;
  }
]
