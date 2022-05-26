{
  description = "Piaf Nix Flake";

  inputs.anmonteiro.url = "github:anmonteiro/nix-overlays";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware";

  outputs = { self, nixpkgs, anmonteiro, nixos-hardware }:
    let system = "x86_64-linux";
    in
    {
      legacyPackages = nixpkgs.legacyPackages;
      nixosConfigurations = import ./nixos-configurations.nix
        {
          inherit nixos-hardware;
          nixosSystem = nixpkgs.lib.nixosSystem;
          pkgs = anmonteiro.legacyPackages."${system}";
        };
    };
}
