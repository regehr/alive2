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

      arm-tests-dir = nixpkgs.lib.cleanSourceWith {
        filter = name: type: nixpkgs.lib.hasSuffix ".ll" name;
        src = ~/Downloads/arm-tests;
      };

    in {
      packages = forAllSystems (sys: pac-nix:
        let pkgs = nixpkgs.legacyPackages.${sys};
        in {
        default =
          pac-nix.alive2-aslp.overrideAttrs {
            name = "alive2-local-build";
            src = pkgs.lib.cleanSourceWith {
              # exclude tests directory from build for faster copy
              filter = name: type: !(baseNameOf name == "tests" || baseNameOf name == "slurm");
              src = ./.;
            };
          };

        arm-tests = 
          pkgs.runCommand "arm-tests" {} ''
            mkdir $out
            ${pkgs.lib.getExe pkgs.rsync} -rz ${arm-tests-dir}/. $out
            find $out -name '*.ll' | sed 's/\.ll$/.bc/' | xargs touch
          '';
      });
    };
}
