{ ihp }:

let
  dontCheckPackages = [
    "cabal-helper"
    "generic-lens"
    "filesystem-conduit"
    "tz"
    "typerep-map"
    "trifecta"
    "hackage-security"
  ];

  doJailbreakPackages = [
    "filesystem-conduit"
    "http-media"
    "aeson"
    "wreq"
    "ghcide"
    "brittany"
  ];

  dontHaddockPackages = [];

  nixPkgsRev = "c985bf793e6ab7d54a9182381b4b610fe0ae6936";
  nixPkgsSha256 = "0zsj9imjbnhkb65r169xxqmjgqd5593insrvncvabg1iqdsrcxz1";

  compiler = "ghc883";

  generatedOverrides = haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = dir: file: _: {
        name = builtins.replaceStrings [ ".nix" ] [ "" ] file;

        value = haskellPackagesNew.callPackage ("${dir}/${file}") {};
      };
      makePackageSet = dir: pkgs.lib.mapAttrs' (toPackage dir) (builtins.readDir dir);
    in
      { "ihp" = ((haskellPackagesNew.callPackage "${ihp}/ihp.nix") { }); } // (makePackageSet ./haskell-packages/.) // (makePackageSet "${ihp}/NixSupport/haskell-packages/.");

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
      let
        toPackage = name: {
          inherit name;

          value = function haskellPackagesOld.${name};
        };
      in
      builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});


  # More exotic overrides go here
  manualOverrides = haskellPackagesNew: haskellPackagesOld: {
    haskell-language-server = haskellPackagesOld.haskell-language-server.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    hls-plugin-api = haskellPackagesOld.hls-plugin-api.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    yaml = haskellPackagesOld.yaml.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    lsp-test = haskellPackagesOld.lsp-test.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    haskell-lsp-types = haskellPackagesOld.haskell-lsp-types.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    haskell-lsp = haskellPackagesOld.haskell-lsp.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    aeson-pretty = haskellPackagesOld.aeson-pretty.overrideScope ( self: super: { aeson = pkgs.haskell.lib.dontCheck haskellPackagesNew.aeson_1_5_2_0; } );
    aeson = pkgs.haskell.lib.dontCheck haskellPackagesOld.aeson_1_5_2_0;
  };

  #mkDerivation = args: super.mkDerivation (args // {
  #    enableLibraryProfiling = true;
  #});
  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" =
          pkgs.haskell.packages."${compiler}".override {
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
        }
        ;
      }
      ;
    };
  };


  pkgs = (import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = nixPkgsRev;
    sha256 = nixPkgsSha256;
  })) { inherit config; };

in
pkgs
