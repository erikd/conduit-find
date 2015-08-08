{ mkDerivation, attoparsec, base, bytestring, conduit-combinators
, conduit-extra, directory, doctest, either, exceptions, filepath
, hspec, mmorph, monad-control, mtl, posix-paths, regex-posix
, semigroups, stdenv, system-filepath, text, time, transformers
, transformers-base, unix, unix-compat
}:
mkDerivation {
  pname = "find-conduit";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring conduit-combinators conduit-extra either
    exceptions filepath mmorph monad-control mtl posix-paths
    regex-posix semigroups system-filepath text time transformers
    transformers-base unix unix-compat
  ];
  testDepends = [
    base conduit-combinators directory doctest filepath hspec
    semigroups
  ];
  description = "A file-finding conduit that allows user control over traversals";
  license = stdenv.lib.licenses.mit;
}
