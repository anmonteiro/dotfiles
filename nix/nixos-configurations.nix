{ nixosSystem, pkgs, nixos-hardware }:

{
  nixpad =
    nixosSystem {
      system = "x86_64-linux";
      modules = [
        ({ lib, ... }:
          {
            nixpkgs = {
              config = {
                allowUnfree = true;
                packageOverrides = _: pkgs;
              };
            };
          })
        # Include the results of the hardware scan.
        ./hardware-configuration.nix

        # Include NixOS hardware quirks
        "${nixos-hardware}/lenovo/thinkpad/t480s"
        "${nixos-hardware}/common/pc/laptop/ssd"

        ./configuration.nix
      ];
    };
}
