{ mkDerivation, attoparsec, base, conduit, conduit-combinators
, conduit-extra, directory, doctest, either, exceptions, filepath
, hspec, mmorph, monad-control, mtl, regex-posix, semigroups
, stdenv, streaming-commons, system-filepath, text, time
, transformers, transformers-base, unix, unix-compat
}:
mkDerivation {
  pname = "find-conduit";
  version = "0.4.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec base conduit conduit-combinators conduit-extra either
    exceptions filepath mmorph monad-control mtl regex-posix semigroups
    streaming-commons system-filepath text time transformers
    transformers-base unix unix-compat
  ];
  testDepends = [
    attoparsec base conduit conduit-combinators directory doctest
    either exceptions filepath hspec mmorph monad-control mtl
    regex-posix semigroups streaming-commons system-filepath text time
    transformers transformers-base unix-compat
  ];
  description = "A file-finding conduit that allows user control over traversals";
  license = stdenv.lib.licenses.mit;
}
