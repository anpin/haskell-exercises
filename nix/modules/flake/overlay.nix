{ inputs, ... }:
{
  perSystem = { config, pkgs, system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config = { allowUnfree = true; allowBroken = true; };
      overlays = [
        # hyperbole.overlays.default
        (final: prev: {
          haskellPackages = prev.haskellPackages.extend (self: super: {
            retrie = prev.haskell.lib.overrideCabal super.retrie (old: {
              postPatch = (old.postPatch or "") + ''
                substituteInPlace retrie.cabal \
                   --replace "data-default >= 0.7.1 && < 0.8" "data-default >= 0.7.1"
              '';
            });
          });
        })
      ];
    };
  };
}
