{ inputs, ... }:
{
  perSystem =
    { config
    , pkgs
    , system
    , ...
    }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
        overlays = [
          # hyperbole.overlays.default
          (final: prev: {
            haskellPackages = prev.haskellPackages.override {

              all-cabal-hashes = builtins.fetchurl {
                # Update from Hackage at 2025-10-07T09:08:43Z
                url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/f1dc2ff721c341a63b2517606115a10201befd23.tar.gz";
                sha256 = "sha256:1zn0ysjgp5nhg164vkyi26xrybicrw8vhx6fy1bza155sw2frsk4";
              };
              overrides = self: super: {
                retrie = prev.haskell.lib.overrideCabal super.retrie (old: {
                  postPatch = (old.postPatch or "") + ''
                    substituteInPlace retrie.cabal \
                       --replace "data-default >= 0.7.1 && < 0.8" "data-default >= 0.7.1"
                  '';
                });
              };
            };
          })
        ];
      };
    };
}
