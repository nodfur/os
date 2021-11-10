self: super: with self; {
  python-openai =
    python3Packages.buildPythonPackage rec {
      pname = "openai";
      version = "0.9.3";

      src = fetchFromGitHub {
        owner = "openai";
        repo = "openai-python";
        rev = "v${version}";
        sha256 = "sha256-JW0l9XPcyTH1bkwr7+9wfUhMEH0XJINzMD6PpyBqUFM=";
      };

      doCheck = false; # didn't work

      propagatedBuildInputs = with python3Packages; [
        requests
        tqdm
        pandas
        pandas-stubs
        openpyxl
      ];
    };

  pandas-stubs =
    python3Packages.buildPythonPackage rec {
      pname = "pandas-stubs";
      version = "1.1.0.12";

      src = python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "sha256-3kOGOqFczwXKk+zz97oGQ8O8osQz0mBQXPsrfKT6fCM=";
      };
    };
}
