{
  description = "DocLog builds documentation from source code in Prolog";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      with pkgs; rec {
        packages = rec {
          default = doclog;

          teruel.src = fetchFromGitHub {
            owner = "aarroyoc";
            repo = "teruel";
            rev = "v1.0.1";
            hash = "sha256-Wi8Z3H53xsegMByW69IskLxJhMmLkRVcLnQAPdFmL5Q=";
          };

          djota.src = fetchFromGitHub {
            owner = "aarroyoc";
            repo = "djota";
            rev = "v0.1.3";
            hash = "sha256-GX4R+eI2AfgnVwh2iNR9Hs5UGoG9pKLnD0bybqb8MQk=";
          };

          doclog = stdenv.mkDerivation {
            pname = "doclog";
            version = "0.0.1";
            src = ./.;
            dontBuild = true;

            installPhase = ''
              mkdir -p $out; cd $out
              cp -r ${teruel.src} $out/teruel
              cp -r ${djota.src} $out/djota
              cp -r ${scryer-prolog.src} $out/scryer-prolog
              cp $src/* $out
              sed -i 's@scryer-prolog@${scryer-prolog}/bin/scryer-prolog@' doclog.sh
              chmod +x doclog.sh
              ln doclog.sh doclog
            '';

            homepage = "https://github.com/aarroyoc/doclog";
            license = lib.licenses.bsd3.fullName;
          };
        };

        apps = {
          doclog = {
            type = "app";
            program = "${packages.doclog}/doclog";
          };
        };
        defaultApp = apps.doclog;
      }
    );
}
