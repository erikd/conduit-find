{ cabal, conduit, conduitCombinators, attoparsec, systemFilepath
, unixCompat, text, regexPosix, hspec, time, semigroups, exceptions
, doctest, either, streamingCommons, transformers, filepath, vector
, posixPaths, monadPar, stm, stmConduit, async, liftedAsync
, monadLoops, parallelIo
}:
mkDerivation {
  pname = "find-conduit";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    conduit conduitCombinators attoparsec systemFilepath text
    unixCompat regexPosix hspec time semigroups exceptions
    doctest either streamingCommons transformers filepath vector
    posixPaths monadPar stm stmConduit async liftedAsync
    monadLoops parallelIo
  ];
  testDepends = [
    attoparsec base conduit conduit-combinators directory doctest
    either exceptions filepath hspec mmorph monad-control mtl
    regex-posix semigroups streaming-commons text time transformers
    transformers-base unix-compat
  ];
  description = "A file-finding conduit that allows user control over traversals";
  license = stdenv.lib.licenses.mit;
}
