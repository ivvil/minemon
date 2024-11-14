{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    pkgs = nixpkgs.legacyPackages."x86_64-linux";
  in {
    devShells."x86_64-linux".default = let
      libs = with pkgs; [openssl];
    in
      pkgs.mkShell {
        packages = with pkgs; [
          openssl
        ];

        shellHook = ''
          export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath libs}:$LD_LIBRARY_PATH
        '';
      };
  };
}
