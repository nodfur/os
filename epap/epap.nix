{
  bashInteractive,
  coreutils,
  feh,
  freetype,
  gdb,
  harfbuzz,
  libpng,
  nodfur-emacs,
  nodfur-emacs-packages,
  openssl,
  pkg-config,
  pstree,
  restless-git,
  rlwrap,
  sbcl,
  stdenv,
  texlive,
  writeShellScriptBin,
  writeText,
  zig
}:

let
  epap-emacs = writeShellScriptBin "epap-emacs" ''
    exec ${nodfur-emacs}/bin/nodfur-emacs boot.lisp --execute "(slime)" "$@"
  '';

  latex = texlive.combine {
    inherit (texlive)
      crop
      dvipng
      ebgaramond
      etoolbox
      extsizes
      fontaxes
      geometry
      parskip
      scheme-basic
      titlesec
      xkeyval
    ;
  };

in stdenv.mkDerivation {
  name = "epap";
  version = "0.5";
  src = ./.;
  buildInputs = [
    bashInteractive
    coreutils
    epap-emacs
    feh
    gdb
    latex
    libpng
    nodfur-emacs
    openssl
    pkg-config
    pstree
    restless-git
    rlwrap
    sbcl
    zig
  ];

  EMACS_SITE_LISP = "${nodfur-emacs-packages.slime}/share/emacs/site-lisp";

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
