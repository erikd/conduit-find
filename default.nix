{ cabal, conduit, conduitCombinators, attoparsec, systemFilepath
, unixCompat, text, regexPosix, profunctors, hspec, time
}:

cabal.mkDerivation (self: {
  pname = "find-conduit";
  version = "0.0.1";
  src = ./.;
  buildDepends = [
    conduit conduitCombinators attoparsec systemFilepath
    unixCompat text regexPosix profunctors hspec time
  ];
  meta = {
    homepage = "https://github.com/yesodweb/Shelly.hs";
    description = "shell-like (systems) programming in Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
    maintainers = [ self.stdenv.lib.maintainers.andres ];
  };
})
