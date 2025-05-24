rec {
  sources = import ./sources.nix;
  defaultNixpkgs = import sources.nixpkgs;
  pkgSetForSystem = system: args: defaultNixpkgs (args // { inherit system; });
  pkgSet = pkgSetForSystem builtins.currentSystem;
}
