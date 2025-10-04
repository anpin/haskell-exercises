# Configuration for the project's Nix devShell
# You mostly want the `packages` option below.
{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }: {
    # Default shell.
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = { allowUnfree = true; cudaSupport = true; };
    };

    devShells.default = pkgs.mkShell {
      name = "exercises";
      meta.description = "Haskell development environment";

      # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell # See ./nix/modules/haskell.nix
        config.pre-commit.devShell # See ./nix/modules/formatter.nix
      ];

      # Packages to be added to Nix devShell go here.
      packages = with pkgs; [
        just
        nixd
        ghciwatch
        windsurf
        nil
        nixfmt-rfc-style
      ];
    };
  };
}
