let haskellSrc = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/a9aa3790f542c023c198d01e31bdfecf4ea6e95a.tar.gz";
    haskellNix = import haskellSrc {};
    # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
    pkgs = import
        # haskell.nix provides access to the nixpkgs pins which are used by our CI,
        # hence you will be more likely to get cache hits when using these.
        # But you can also just use your own, e.g. '<nixpkgs>'.
        haskellNix.sources.nixpkgs-unstable
        # These arguments passed to nixpkgs, include some patches and also
        # the haskell.nix functionality itself as an overlay.
        haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.snapshots."lts-20.26".ghcWithPackages ( ps: with ps; [
    base
    containers
    composition-extra
    text
    unordered-containers
    split
    vector
])
