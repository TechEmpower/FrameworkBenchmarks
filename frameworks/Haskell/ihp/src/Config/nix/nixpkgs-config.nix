{ ihp }:

let
  dontCheckPackages = [
    "ghc-mod"
    "cabal-helper"
    "generic-lens"
    "filesystem-conduit"
    "tz"
    "typerep-map"
  ];

  doJailbreakPackages = [
    "ghc-mod"
    "filesystem-conduit"
    "http-media"
  ];

  dontHaddockPackages = [];

  nixPkgsRev = "da7ddd822e32aeebea00e97ab5aeca9758250a40";
  nixPkgsSha256 = "0zbxbk4m72psbvd5p4qprcpiadndq1j2v517synijwp2vxc7cnv6";

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
    ihp = pkgs.haskell.lib.allowInconsistentDependencies haskellPackagesOld.ihp;
    time_1_9_3 = pkgs.haskell.lib.dontCheck haskellPackagesOld.time_1_9_3;
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
