{ mkDerivation, aeson, base, bytestring, deepseq, hashable, hspec
, lib, parameterized-utils, prettyprinter, sayable, tasty
, tasty-ant-xml, tasty-checklist, tasty-hspec, template-haskell
, text, unordered-containers
}:
mkDerivation {
  pname = "named-text";
  version = "1.2.5.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson base deepseq hashable prettyprinter sayable template-haskell
    text
  ];
  testHaskellDepends = [
    aeson base bytestring hspec parameterized-utils prettyprinter
    sayable tasty tasty-ant-xml tasty-checklist tasty-hspec text
    unordered-containers
  ];
  description = "A parameterized named text type and associated functionality";
  license = lib.licenses.isc;
}
