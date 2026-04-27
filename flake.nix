{
  description = "Piaf Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    neovim-src = {
      url = "github:neovim/neovim/master";
      flake = false;
    };
    fff = {
      url = "github:dmtrKovalenko/fff.nvim";
      inputs.nixpkgs.follows = "nixpkgs/nixpkgs";
      inputs.zig-overlay.inputs.systems.follows = "fff/flake-utils/systems";
    };
    fff-cli = {
      url = "github:anmonteiro/fff-cli";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fff.follows = "fff";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      nixos-hardware,
      neovim-src,
      fff,
      fff-cli,
    }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system}.extend (
              self: super: {
                neovim-src = neovim-src;
                fff = fff.outputs.packages.${system}.default.overrideAttrs (_: {
                  postInstall = ''
                    ln -sfn $out/lib/ $out/release
                  '';
                });
                fff-cli = fff-cli.packages.${system}.default;
              }
            );
          in
          f pkgs
        );
    in
    let
      system = "x86_64-linux";
    in
    rec {
      legacyPackages = forAllSystems (pkgs: pkgs);
      nixosConfigurations = import ./nix/nixos-configurations.nix {
        inherit nixos-hardware;
        nixosSystem = nixpkgs.lib.nixosSystem;
        pkgs = legacyPackages."${system}";
      };
    };
}
