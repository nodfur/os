{
  stdenv,

  pkg-config,
  openssl,
  libpng,

  sbcl,
  zig,

  nodfur-emacs,   # our own Emacs configuration
  nodfur-emacs-packages,
}:

stdenv.mkDerivation {
  name = "epap-light";
  version = "0.5";
  src = ./.;
  buildInputs = [
    pkg-config
    openssl
    libpng
    sbcl
    zig
    nodfur-emacs
  ];

  EMACS_SITE_LISP = "${nodfur-emacs-packages.slime}/share/emacs/site-lisp";

  preBuild = ''
    export HOME=$TMPDIR
  '';

  installPhase = ''
    # zig build --prefix $out install
  '';
}
