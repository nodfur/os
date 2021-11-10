self: super: with self; {
  tdlib-json-cli = stdenv.mkDerivation rec {
    pname = "tdlib-json-cli";
    version = "1.7.0";
    src = tdlib-json-cli-src;
    nativeBuildInputs = [cmake];
    buildInputs = [tdlib];
    configurePhase = "cmake .";
    installPhase = ''
      mkdir -p $out/bin
      cp bin/tdlib_json_cli $out/bin/tdlib-json-cli
    '';
  };
}
