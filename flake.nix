{
  description = "Some random bits of code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
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
                    checkPhase = ''
                      ghc-pkg --package-db=$packageConfDir list
                      make PACKAGE_DB=$packageConfDir test
                    '';
                  });
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
      }
    );
}
