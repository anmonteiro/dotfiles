{
  description = "Piaf Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    fff = {
      url = "github:dmtrKovalenko/fff.nvim";
      # inputs.nixpkgs.follows = "nixpkgs";
      # inputs.rust-overlay.follows = "rust-overlay";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      nixos-hardware,
      fff,
    }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system}.extend (
              self: super: {
                fff = fff.outputs.packages.${system}.default.overrideAttrs (_: {
                  postInstall = ''
                    ln -sfn $out/lib/ $out/release
                  '';
                });
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
