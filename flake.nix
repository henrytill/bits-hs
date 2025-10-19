{
  description = "Some random bits of code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    inspection-testing-src = {
      url = "github:nomeata/inspection-testing/0.6.2";
      flake = false;
    };
  };

  nixConfig = {
    extra-substituters = [ "https://henrytill.cachix.org" ];
    extra-trusted-public-keys = [
      "henrytill.cachix.org-1:EOoUIk8e9627viyFmT6mfqghh/xtfnpzEtqT4jnyn1M="
    ];
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      inspection-testing-src,
      ...
    }:
    let
      ghcName = "ghc967";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcName} = prev.haskell.packages.${ghcName}.override {
              overrides = hfinal: hprev: {
                bits =
                  let
                    src = builtins.path {
                      path = ./.;
                      name = "bits-src";
                    };
                    bits = hfinal.callCabal2nix "bits" src { };
                  in
                  final.haskell.lib.overrideCabal bits (_: {
                    doHaddock = false;
                    testToolDepends = [ hfinal.doctest ];
                    preHaddock = ''
                      ghc-pkg --package-db=$packageConfDir list
                      make PACKAGE_DB=$packageConfDir test
                    '';
                  });
                inspection-testing = hfinal.callCabal2nix "inspection-testing" inspection-testing-src { };
              };
            };
          };
        };
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages.bits = pkgs.haskell.packages.${ghcName}.bits;
        packages.default = self.packages.${system}.bits;
        devShells.default = pkgs.haskell.packages.${ghcName}.shellFor {
          packages = hpkgs: [ hpkgs.bits ];
          withHoogle = true;
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskell.packages.${ghcName}.fourmolu
            haskell.packages.${ghcName}.ghc-tags
            haskell.packages.${ghcName}.hlint
          ];
        };
      }
    );
}
