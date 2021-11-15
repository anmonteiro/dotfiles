{
  description = "Piaf Nix Flake";

  # inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";
  inputs.anmonteiro.url = "/home/anmonteiro/projects/nix-overlays";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware";
  # inputs.anmonteiro.url = "github:anmonteiro/nix-overlays";

  outputs = { self, nixpkgs, anmonteiro, nixos-hardware }:
    let system = "x86_64-linux";
    in
    {
      nixosConfigurations = import ./nixos-configurations.nix
        {
          inherit nixos-hardware;
          nixosSystem = nixpkgs.lib.nixosSystem;
          pkgs = anmonteiro.legacyPackages."${system}";
        };
    };
}
