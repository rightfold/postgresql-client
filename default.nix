{pkgs ? import ./nix/pkgs.nix {}}:
pkgs.haskellPackages.callPackage ./postgresql-client.nix {}
