# haskell-flake configuration goes in this module.

{ root, inputs, ... }:
let patches = root + /patches; in
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
        packages = rec {
          # hyperbole.source = "0.5.0";
          skeletest.source = "0.1.0";
          # hyperbole.source = hyperbole.packages.hyperbole;
          # web-view = {};
          # hlint.source = "3.10";
          # ghc-lib-parser.source = "9.12.2.20250421";
          # ghc-lib-parser-ex.source = "9.12.0.0";
          # ormolu.source = "0.8.0.2";
          # skeletest.source = "0.2.1";
          # Diff.source = "1.0.2";
          # fourmolu.source = "0.17.0.0";
          # atomic-css.source = "0.2.0";
          # hyperbole.source = ./hyperbole.nix;
          # http-client-tls.source = "0.3.6.4";
          tls.source = "2.1.6";
        };
        settings = {
          skeletest.broken = true;
          web-view = { super, ... }: { custom = _: super.atomic-css; };
          hyperbole = { super, ... }: { custom = _: super.callPackage (patches + /hyperbole.nix) { }; };
          data-default = { super, ... }: { custom = _: super.data-default_0_8_0_1; };
          atomic-css = { super, ... }: {
            custom = _: super.callPackage (patches + /atomic-css.nix) { };
            patches = [
              (patches + /atomic-css.patch)
            ];
          };
          floskell = {
            patches = [
              (patches + /floskell.patch)
            ];
          };
          hoogle = {
            patches = [
              (patches + /hoogle.patch)
            ];
          };
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
