# haskell-flake configuration goes in this module.

{ root, inputs, ... }:
let
  patches = root + /patches;
in
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem =
    { self'
    , lib
    , config
    , pkgs
    , ...
    }:
    {
      haskellProjects.default = {
        projectFlakeName = "haskell-multi-nix";
        # basePackages = pkgs.haskell.packages.ghc910;
        projectRoot = builtins.toString (
          lib.fileset.toSource {
            inherit root;
            fileset = lib.fileset.unions [
              (root + /exercises)
              (root + /game-of-life)
              (root + /cabal.project)
              (root + /LICENSE)
              (root + /README.md)
            ];
          }
        );
        packages = {
          hyperbole.source = "0.5.0";
          skeletest.source = "0.1.0";
          atomic-css.source = "0.2.0";
          tls.source = "2.1.6";

        };
        settings = {
          skeletest.broken = true;
          data-default =
            { super, ... }:
            {
              custom = _: super.data-default_0_8_0_1;
            };
          atomic-css.patches = [
            (patches + /atomic-css.patch)
          ];
          floskell.patches = [
            (patches + /floskell.patch)
          ];
          hoogle.patches = [
            (patches + /hoogle.patch)
          ];
        };

        # What should haskell-flake add to flake outputs?
        autoWire = [
          "packages"
          "apps"
          "checks"
        ]; # Wire all but the devShell
      };

      # Default package & app.
      packages.default = self'.packages.exercises;
      apps.default = self'.apps.exercises;
    };
}
