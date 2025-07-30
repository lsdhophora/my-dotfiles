{
  description = "A simple NixOS flake";

  nixConfig = {
    trusted-substituters = ["https://mirrors.ustc.edu.cn/nix-channels/store"];
 };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, agenix, ... }@inputs: {
    nixosConfigurations.flowerpot = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
       agenix.nixosModules.default
       ./configuration.nix
      ];
    specialArgs = { inherit inputs; };
    };
  };
}
