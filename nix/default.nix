rec {
  chooseSources =
    if builtins.pathExists /home/dan/pro/nixpkgs
    then ./local.sources.json
    else
      if builtins.pathExists ./sources.json
      then ./sources.json
      else null;
  sources = import ./sources.nix { inherit chooseSources; };
  defaultNixpkgs = import sources.nixpkgs;
  pkgSetForSystem = system: args: defaultNixpkgs (args // { inherit system; });
  pkgSet = pkgSetForSystem builtins.currentSystem;
}
