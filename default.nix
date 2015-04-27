{ mkDerivation, attoparsec, base, bytestring, directory, doctest
, either, exceptions, filepath, hspec, mmorph, monad-control, mtl
, posix-paths, regex-posix, semigroups, simple-conduit, stdenv
, system-filepath, text, time, transformers, transformers-base
, unix, unix-compat
}:
mkDerivation {
  pname = "find-conduit";
  version = "0.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    attoparsec base bytestring either exceptions filepath mmorph
    monad-control mtl posix-paths regex-posix semigroups simple-conduit
    system-filepath text time transformers transformers-base unix
    unix-compat
  ];
  testDepends = [
    base directory doctest filepath hspec semigroups simple-conduit
  ];
  description = "A file-finding conduit that allows user control over traversals";
  license = stdenv.lib.licenses.mit;
}
