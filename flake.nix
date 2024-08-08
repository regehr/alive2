{ 
  nixConfig.extra-substituters = [ "https://pac-nix.cachix.org/" ];
  nixConfig.extra-trusted-public-keys = [ "pac-nix.cachix.org-1:l29Pc2zYR5yZyfSzk1v17uEZkhEw0gI4cXuOIsxIGpc=" ];

  inputs.pac-nix.url = "github:katrinafyi/pac-nix";

  outputs = {self, pac-nix}:
    let
      nixpkgs = pac-nix.inputs.nixpkgs;

      forAllSystems = f:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: f system pac-nix.legacyPackages.${system});

    in {
      packages = forAllSystems (sys: pac-nix: {
        default =
          pac-nix.alive2-aslp.overrideAttrs {
            name = "alive2-local-build";
            src = nixpkgs.lib.cleanSourceWith {
              # exclude tests directory from build for faster copy
              filter = name: type: !(baseNameOf name == "tests" || baseNameOf name == "slurm");
              src = ./.;
            };
          };
      });
    };
}
