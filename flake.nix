{
  description = "Piaf Nix Flake";

  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware";
  inputs.nixos-hardware.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, nixos-hardware }:
    let system = "x86_64-linux";
    in
    rec {
      legacyPackages = nixpkgs.legacyPackages;
      nixosConfigurations = import ./nixos-configurations.nix
        {
          inherit nixos-hardware;
          nixosSystem = nixpkgs.lib.nixosSystem;
          pkgs = legacyPackages."${system}";
        };
    };
}
