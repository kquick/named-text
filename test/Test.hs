{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import qualified Data.HashMap.Strict as Map
import           Data.Name
import           Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import           Data.Proxy ( Proxy(Proxy) )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text )
import qualified Data.Text as T
import           GHC.Exts ( proxy#, IsList(fromList, toList) )
import           GHC.Generics ( Generic )
import qualified Prettyprinter as PP
import           Text.Sayable

import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Checklist
import           Test.Tasty.Hspec
import           Test.Tasty.Runners.AntXML

#ifdef VERSION_aeson
import           Data.Aeson
import           Data.ByteString.Lazy ( ByteString )
import           Data.Name.JSON
#endif


main :: IO ()
main = tests >>= defaultMainWithIngredients (antXMLRunner : defaultIngredients)

tests :: IO TestTree
tests = testGroup "Named" <$> sequence
        [
          testCreate
        , testRender
        , testSemigroup
        , testIsList
        , testConversions
        , testValidNames
        , testSomeNames
        , testHasName
        , testUtilities
#ifdef VERSION_aeson
        , testJSON
#endif
        ]


instance TestShow Text
instance Sayable "test" (Named s n)
         => TestShow (Named s n) where testShow = sez @"test"
instance TestShow (Proxy UTF8) where testShow _ = "Proxy :: \"UTF8\""
instance TestShow (Proxy CaseInsensitive) where testShow _ = "Proxy :: !Case"
instance TestShow (Proxy CaseInsensitivePreserve) where testShow _ = "Proxy :: !Case+Preserved"
instance TestShow (Proxy Secure) where testShow _ = "Proxy :: Secure"
instance TestShow [SomeName] where testShow = testShowList
instance TestShow SomeName where testShow = viewSomeName testShow


testCreate :: IO TestTree
testCreate = testSpec "Named Creation" $
  describe "creation, properties, and extraction of Named" $ do

    it "CR1 creates UTF8 from IsText" $
      withChecklist "CR1" $
      fromText @(Named UTF8 "CR1") ("test text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test text"
       :> Got "case sensitive text value" (/= "Test Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR1"
       :> Val "style proxy" styleProxy (Proxy :: Proxy "UTF8")
       :> Val "extracted text" nameText ("test text" :: Text)
      )

    it "CR2 creates UTF8 from IsString" $
      withChecklist "CR2" $
      fromString @(Named UTF8 "CR2") ("test string" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "test string"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy "UTF8")
       :> Val "extracted text" nameText ("test string" :: Text)
      )

    it "CR3 creates CaseInsensitive from IsText" $
      withChecklist "CR3" $
      fromText @(Named CaseInsensitive "CR3") ("Test teXT" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "tesT tExt"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR3"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
       :> Val "extracted text" nameText ("test text" :: Text)
      )

    it "CR4 creates CaseInsensitive from IsString" $
      withChecklist "CR4" $
      fromString @(Named CaseInsensitive "CR4") ("TEst STring" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "teST sTring"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR4"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
       :> Val "extracted text" nameText ("test string" :: Text)
      )

    it "CR5 creates Secure from IsText" $
      withChecklist "CR5" $
      fromText @(Named Secure "CR5") ("test text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test text"
       :> Got "case sensitive secure value" (/= "Test Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR5"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("te#####xt" :: Text)
       :> Val "security bypass text" secureNameBypass ("test text" :: Text)
      )

    it "CR6 creates Secure from IsString" $
      withChecklist "CR6" $
      fromString @(Named Secure "CR6") ("TEst STring" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "TEst STring"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR6"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("TE#######ng" :: Text)
       :> Val "security bypass text" secureNameBypass ("TEst STring" :: Text)
      )

    it "CR7 creates short Secure" $
      withChecklist "CR7" $
      ("z" :: Named Secure "CR7")
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "z"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR7"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
       :> Val "extracted text" nameText ("########" :: Text)
       :> Val "security bypass text" secureNameBypass ("z" :: Text)
      )

    it "CR8 creates CaseInsensitivePreserve from IsText" $
      withChecklist "CR8" $
      fromText @(Named CaseInsensitivePreserve "CR8") ("Test teXT" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "tesT tExt"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR8"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitivePreserve)
       :> Val "extracted text" nameText ("Test teXT" :: Text)
      )

    it "CR9 creates CaseInsensitivePreserve from IsString" $
      withChecklist "CR9" $
      fromString @(Named CaseInsensitivePreserve "CR9") ("TEst STring" :: String)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent string value" id "teST sTring"
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR9"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitivePreserve)
       :> Val "extracted text" nameText ("TEst STring" :: Text)
      )


testRender :: IO TestTree
testRender = testSpec "Named Rendering" $
  describe "rendering of Named" $ do

  it "CR10 render UTF8 via Sayable" $
    withChecklist "CR10" $
    ("test text" :: Name "CR10")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR10 'test text'"
     :> Val "as sayable info" (sez @"info") "test text"
    )

  it "CR11 render UTF8 via Prettyprinter" $
    withChecklist "CR11" $
    fromText @(Named "UTF8" "CR11") ("test of text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR11 'test of text'"
    )

  it "CR12 render UTF8 via Show" $
    withChecklist "CR12" $
    fromText @(Named "UTF8" "CR12") ("test of text" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR12 'test of text'"
    )

  it "CR13 render CaseInsensitive via Sayable" $
    withChecklist "CR13" $
    ("tEST TeXt" :: Named CaseInsensitive "CR13")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR13 «test text»"
     :> Val "as sayable info" (sez @"info") "test text"
    )

  it "CR14 render CaseInsensitive via Prettyprinter" $
    withChecklist "CR14" $
    fromText @(Named CaseInsensitive "CR14") ("test OF text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR14 «test of text»"
    )

  it "CR15 render CaseInsensitive via Show" $
    withChecklist "CR15" $
    fromText @(Named CaseInsensitive "CR15") ("TEST OF TEXT" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR15 «test of text»"
    )

  it "CR16 render Secure via Sayable" $
    withChecklist "CR16" $
    ("tEST TeXt" :: Named Secure "CR16")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR16 'tE#####Xt'"
     :> Val "as sayable info" (sez @"info") "tE#####Xt"
    )

  it "CR17 render Secure via Prettyprinter" $
    withChecklist "CR17" $
    fromText @(Named Secure "CR17") ("test OF text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR17 'te########xt'"
    )

  it "CR18 render Secure via Show" $
    withChecklist "CR18" $
    fromText @(Named Secure "CR18") ("TEST OF TEXT" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR18 'TE########XT'"
    )

  it "CR19 render short Secure via Sayable" $
    withChecklist "CR19" $
    ("X" :: Named Secure "CR19")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR19 '########'"
     :> Val "as sayable info" (sez @"info") "########"
    )

  it "CR13 render CaseInsensitive via Sayable" $
    withChecklist "CR13" $
    ("tEST TeXt" :: Named CaseInsensitive "CR13")
    `checkValues`
    (Empty
     :> Val "as sayable" (sez @"test") "CR13 «test text»"
     :> Val "as sayable info" (sez @"info") "test text"
    )

  it "CR19.1 render CaseInsensitivePreserve via Prettyprinter" $
    withChecklist "CR19.1" $
    fromText @(Named CaseInsensitivePreserve "CR19.1") ("test OF text" :: Text)
    `checkValues`
    (Empty
     :> Val "as pretty" (show . PP.pretty) "CR19.1 «test OF text»"
    )

  it "CR19.2 render CaseInsensitive via Show" $
    withChecklist "CR19.2" $
    fromText @(Named CaseInsensitive "CR19.2") ("TEST OF TEXT" :: Text)
    `checkValues`
    (Empty
     :> Val "as show" show "CR19.2 «test of text»"
    )


testSemigroup :: IO TestTree
testSemigroup = testSpec "Named Semigroup" $
  describe "semigroup of Named" $ do

  it "CR20 UTF8 semigroup" $
    withChecklist "CR20" $
    (fromText @(Named UTF8 "CR20") ("more test text" :: Text)
     <> " and still more")
    `checkValues`
    (Empty
     :> Val "raw value" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR20 'more test text and still more'"
    )

  it "CR21 CaseInsensitive semigroup" $
    withChecklist "CR21" $
    (fromText @(Named CaseInsensitive "CR21") ("mORE teST TexT" :: Text)
     <> " and STILL more")
    `checkValues`
    (Empty
     :> Val "implicitly constructed full form" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR21 «more test text and still more»"
    )

  it "CR22 Secure semigroup" $
    withChecklist "CR22" $
    (fromText @(Named Secure "CR22") ("more test text" :: Text)
     <> " and still more")
    `checkValues`
    (Empty
     :> Val "implicitly constructed full form" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR22 'mo#########################re'"
    )

  it "CR23 CaseInsensitivePreserve semigroup" $
    withChecklist "CR23" $
    (fromText @(Named CaseInsensitivePreserve "CR23") ("mORE teST TexT" :: Text)
     <> " and STILL more")
    `checkValues`
    (Empty
     :> Val "implicitly constructed full form" id "more test text and still more"
     :> Val "as sayable" (sez @"test") "CR23 «mORE teST TexT and STILL more»"
    )

testIsList :: IO TestTree
testIsList = testSpec "Named IsList" $
  describe "IsList of Named" $ do

  it "CR30 UTF8 IsList" $
    withChecklist "CR30" $
    (fromList "list of text" :: Name "CR30")
    `checkValues`
    (Empty
     :> Val "matches implicit construction" id "list of text"
     :> Val "as sayable" (sez @"test") "CR30 'list of text'"
     :> Val "as extracted text" nameText "list of text"
     :> Val "as list" toList ['l','i','s','t',' ','o','f',' ','t','e','x','t']
    )

  -- Note: no IsList instance for CaseInsensitive or or CaseInsensitivePreserve or Secure

testConversions :: IO TestTree
testConversions = testSpec "Named Conversions" $ do

  describe "Named nameOf conversions" $ do
    -- n.b. these tests use the "instance ConvertName" below.

    it "CR40 UTF8 default conversion" $
      withChecklist "CR40" $
      (convertName (fromText "list of text" :: Name "CR40") :: Name "CR40-2")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "list of text"
       :> Val "as sayable" (sez @"test") "CR40-2 'list of text'"
       :> Val "as extracted text" nameText "list of text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR40-2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR41 UTF8 explicit conversion" $
      withChecklist "CR41" $
      (convertName (fromText "list of text" :: Name "CR41") :: Name "CR41-3")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "mjtu!pg!ufyu"
       :> Val "as sayable" (sez @"test") "CR41-3 'mjtu!pg!ufyu'"
       :> Val "as extracted text" nameText "mjtu!pg!ufyu"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR41-3"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR42 CaseInsensitive conversion" $
      withChecklist "CR42" $
      (convertName (fromText "biT OF teXt" :: Named CaseInsensitive "CR42")
       :: Named CaseInsensitive "CR42 new")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "bit of text"
       :> Val "as sayable" (sez @"test") "CR42 new «bit of text»"
       :> Val "as extracted text" nameText "bit of text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR42 new"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
      )

    it "CR43 Secure conversion" $
      withChecklist "CR43" $
      (convertName (fromText "hidden text" :: Named Secure "CR43")
       :: Named Secure "CR43 again")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "hidden text"
       :> Val "as sayable" (sez @"test") "CR43 again 'hi#######xt'"
       :> Val "as extracted text" nameText "hi#######xt"
       :> Val "security bypass extraction" secureNameBypass ("hidden text" :: Text)
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR43 again"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
      )

    it "CR42.1 CaseInsensitivePreserve conversion" $
      withChecklist "CR42.1" $
      (convertName (fromText "biT OF teXt" :: Named CaseInsensitivePreserve "CR42.1")
       :: Named CaseInsensitivePreserve "CR42.1 new")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "bit of text"
       :> Val "as sayable" (sez @"test") "CR42.1 new «biT OF teXt»"
       :> Val "as extracted text" nameText "biT OF teXt"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR42.1 new"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitivePreserve)
      )

  describe "Named style conversions" $ do
    -- n.b. these tests use the "instance ConvertNameStyle" below.

    it "CR44 UTF8->CaseInsensitive default conversion" $
      withChecklist "CR44" $
      (convertStyle (fromText "Some TEXT" :: Name "CR44")
        :: Named CaseInsensitive "CR44")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some text"
       :> Val "as sayable" (sez @"test") "CR44 «some text»"
       :> Val "as extracted text" nameText "some text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR44"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitive)
      )

    it "CR44.1 UTF8->CaseInsensitivePreserve default conversion" $
      withChecklist "CR44.1" $
      (convertStyle (fromText "Some TEXT" :: Name "CR44.1")
        :: Named CaseInsensitivePreserve "CR44.1")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some text"
       :> Val "as sayable" (sez @"test") "CR44.1 «Some TEXT»"
       :> Val "as extracted text" nameText "Some TEXT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR44.1"
       :> Val "style proxy" styleProxy (Proxy :: Proxy CaseInsensitivePreserve)
      )

    it "CR45 UTF8->Secure default conversion" $
      withChecklist "CR45" $
      (convertStyle (fromText "Some TEXT" :: Name "CR45") :: Named Secure "CR45")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some TEXT"
       :> Val "as sayable" (sez @"test") "CR45 'So#####XT'"
       :> Val "as extracted text" nameText "So#####XT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR45"
       :> Val "style proxy" styleProxy (Proxy :: Proxy Secure)
      )

    it "CR46 CaseInsensitive->UTF8 default conversion" $
      withChecklist "CR46" $
      (convertStyle (fromText "Some TEXT" :: Named CaseInsensitive "CR46")
        :: Named UTF8 "CR46")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some text" -- Note: converted to lowercase
       :> Val "as sayable" (sez @"test") "CR46 'some text'"
       :> Val "as extracted text" nameText "some text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR46"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR46.1 CaseInsensitivePreserve->UTF8 default conversion" $
      withChecklist "CR46.1" $
      (convertStyle (fromText "Some TEXT" :: Named CaseInsensitivePreserve "CR46.1")
        :: Named UTF8 "CR46.1")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some TEXT"
       :> Val "as sayable" (sez @"test") "CR46.1 'Some TEXT'"
       :> Val "as extracted text" nameText "Some TEXT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR46.1"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR47 Secure->UTF8 default conversion" $
      withChecklist "CR47" $
      (convertStyle (fromText "Some TEXT" :: Named Secure "CR47") :: Name "CR47")
      `checkValues`
      (Empty
       -- note here that the default only gets the masked form!
       :> Val "matches implicit construction" id "So#####XT"
       :> Val "as sayable" (sez @"test") "CR47 'So#####XT'"
       :> Val "as extracted text" nameText "So#####XT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR47"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

    it "CR48 Secure->UTF8 implicit bypass conversion" $
      withChecklist "CR48" $
      (convertStyle (fromText "Some TEXT" :: Named Secure "CR48") :: Name "CR48")
      `checkValues`
      (Empty
       -- note here that the default only gets the masked form!
       :> Val "matches implicit construction" id "Some TEXT"
       :> Val "as sayable" (sez @"test") "CR48 'Some TEXT'"
       :> Val "as extracted text" nameText "Some TEXT"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR48"
       :> Val "style proxy" styleProxy (Proxy :: Proxy UTF8)
      )

instance ConvertName UTF8 "CR40" "CR40-2"
instance ConvertName UTF8 "CR41" "CR41-3" where
  convertName = fromText . T.map succ . nameText
instance ConvertName CaseInsensitive "CR42" "CR42 new"
instance ConvertName CaseInsensitivePreserve "CR42.1" "CR42.1 new"
instance ConvertName Secure "CR43" "CR43 again" where
  convertName = fromText . secureNameBypass

instance ConvertNameStyle UTF8 Secure "CR45"
instance ConvertNameStyle CaseInsensitive UTF8 "CR46"
instance ConvertNameStyle Secure UTF8 "CR47"
instance ConvertNameStyle Secure UTF8 "CR48" where
  convertStyle = fromText . secureNameBypass


testValidNames :: IO TestTree
testValidNames = testSpec "Named ValidNames" $
  describe "ValidNames" $ do
    -- n.b. Uses the validateName function below.

  it "CR50 Valid Names" $
    withChecklist "CR50" $
    (fromText "valid text" :: Name "CR50")
    `checkValues`
    (Empty
     :> Got "valid name for validateName call" validateName
     :> Val "validated text extraction"
      (validName (Proxy :: Proxy '[ "Foo", "CR50", "Other"])) "valid text"
    )

    -- It would be nice to test the failure case for ValidNames, but that's a
    -- compilation failure.

validateName :: ValidNames n '[ "CR50", "CR50.2" ] => Name n -> Bool
validateName = const True


testSomeNames :: IO TestTree
testSomeNames = testSpec "Named SomeName and SomeStyle" $ do
  describe "SomeName" $ do

    it "CR60 SomeName collection and extraction" $
      withChecklist "CR60" $
      ([ SomeName (fromText "text one" :: Name "CR60.1")
       , SomeName (fromText "text two" :: Name "CR60 2")
       , SomeName (fromText "text 3" :: Name "CR60-three")
       ])
      `checkValues`
      (Empty
       :> Val "list length" length 3
       :> Val "@ 0" (viewSomeName (sez @"test") . (!!0)) "CR60.1 'text one'"
       :> Val "@ 1" (viewSomeName (sez @"test") . (!!1)) "CR60 2 'text two'"
       :> Val "@ 2" (viewSomeName (sez @"test") . (!!2)) "CR60-three 'text 3'"
      )

  describe "SomeStyle" $ do

    it "CR61 SomeStyle collection and extraction" $
      withChecklist "CR61" $
      ([ SomeNameStyle ("Regular text." :: Name "CR61")
       , SomeNameStyle ("cASE inSENsitIVE tEXt" :: Named CaseInsensitive "CR61")
       , SomeNameStyle ("secret text" :: Named Secure "CR61")
       ]
      )
      `checkValues`
      (Empty
       :> Val "list length" length 3
       :> Val "val @ 0" (viewSomeNameStyle nameText . (!!0)) "Regular text."
       :> Val "val @ 1" (viewSomeNameStyle nameText . (!!1)) "case insensitive text"
       :> Val "val @ 2" (viewSomeNameStyle nameText . (!!2)) "se#######xt"
       :> Val "nameOf @ 0" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!0)) "CR61"
       :> Val "nameOf @ 1" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!1)) "CR61"
       :> Val "nameOf @ 2" (viewSomeNameStyle (\n -> nameOf n proxy#) . (!!2)) "CR61"
      )

instance TestShow [SomeNameStyle "CR61"] where testShow = testShowList
instance TestShow (SomeNameStyle "CR61") where
  testShow = viewSomeNameStyle (\n -> nameOf n proxy# <> ": " <> show (nameText n))


testHasName :: IO TestTree
testHasName = testSpec "HasName" $ do
  describe "HasName Foo" $ do

    it "CR70 can extract UTF8 myName from Foo" $
      myName (Foo "bar" "baz" (Map.fromList [("one","quux"), ("two","brox")]))
      `shouldBe` ("baz" :: Name "principle")

    it "CR71 can extract secure myName from Bar" $
      myName (Bar "baz" "quux") `shouldBe` ("quux" :: Named Secure "second")

data Foo = Foo (Name "prefix") (Name "principle")
               (Map.HashMap (Name "alt-key") (Name "alt"))
         deriving (Eq, Generic, Show)
instance HasName Foo UTF8 "principle" where myName (Foo _ p _) = p

data Bar = Bar (Name "first") (Named Secure "second")
instance HasName Bar Secure "second" where myName (Bar _ s) = s

instance TestShow Foo

testUtilities :: IO TestTree
testUtilities = testSpec "Named utilities" $ do
  describe "Named length" $ do

    it "CR80 can get a UTF8 length" $
      nameLength ("Length of TEXT" :: Name "CR80") `shouldBe` 14

    it "CR81 can get a CaseInsensitive length" $
      nameLength ("Length of TEXT" :: Named CaseInsensitive "CR81") `shouldBe` 14

    it "CR81.1 can get a CaseInsensitivePreserve length" $
      nameLength ("Length of TEXT" :: Named CaseInsensitivePreserve "CR81.1") `shouldBe` 14

    it "CR82 can get a Secure length" $
      nameLength ("Length of secure TEXT" :: Named Secure "CR82") `shouldBe` 21

  describe "Named null check" $ do

    it "CR83 can check a null UTF8" $
      nullName ("" :: Name "CR83") `shouldBe` True

    it "CR84 can check a non-null UTF8" $
      nullName ("Not empty" :: Name "CR84") `shouldBe` False

    it "CR85 can check a null CaseInsensitive named" $
      nullName ("" :: Named CaseInsensitive "CR85") `shouldBe` True

    it "CR85.1 can check a null CaseInsensitivePreserve named" $
      nullName ("" :: Named CaseInsensitivePreserve "CR85.1") `shouldBe` True

    it "CR86 can check a non-null CaseInsensitive named" $
      nullName ("Not empty" :: Named CaseInsensitive "CR86") `shouldBe` False

    it "CR86.1 can check a non-null CaseInsensitivePreserve named" $
      nullName ("Not empty" :: Named CaseInsensitivePreserve "CR86.1") `shouldBe` False

    it "CR87 can check a null Secure named" $
      nullName ("" :: Name "CR87") `shouldBe` True

    it "CR88 can check a non-null Secure named" $
      nullName ("Not empty" :: Named Secure "CR88") `shouldBe` False

  describe "Name prefix check" $ do

    it "CR110 can check a null prefix of a null UTF8" $
      (("" :: Name "CR110") `isPrefixOfName` ("" :: Name "CR110")) `shouldBe` True

    it "CR111 can check a null prefix UTF8" $
      (("" :: Name "CR111") `isPrefixOfName` ("stuff" :: Name "CR111"))
      `shouldBe` True

    it "CR112 can check a valid prefix UTF8" $
      (("st" :: Name "CR112") `isPrefixOfName` ("stuff" :: Name "CR112"))
      `shouldBe` True

    it "CR113 can check a long multi-word prefix UTF8" $
      (("This is a\n\t prefix" :: Name "CR113")
       `isPrefixOfName`
       ("This is a\n\t prefix!" :: Name "CR113")) `shouldBe` True

    it "CR114 rejects an invalid prefix UTF8" $
      (("bad" :: Name "CR114") `isPrefixOfName` ("stuff" :: Name "CR114"))
      `shouldBe` False

    it "CR115 rejects an too-long prefix UTF8" $
      (("stuffing" :: Name "CR115") `isPrefixOfName` ("stuff" :: Name "CR115"))
      `shouldBe` False

    it "CR116 rejects an case mismatch UTF8" $
      (("STUFF" :: Name "CR116") `isPrefixOfName` ("stuff" :: Name "CR116"))
      `shouldBe` False

    -----------------------------

    it "CR120 can check a null prefix of a null CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR120")
       `isPrefixOfName`
       ("" :: Named CaseInsensitive "CR120")) `shouldBe` True

    it "CR121 can check a null prefix CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR121")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitive "CR121"))
      `shouldBe` True

    it "CR122 can check a valid prefix CaseInsensitive" $
      (("sT" :: Named CaseInsensitive "CR122")
       `isPrefixOfName`
       ("Stuff" :: Named CaseInsensitive "CR122"))
      `shouldBe` True

    it "CR123 can check a long multi-word prefix CaseInsensitive" $
      (("This is A\n\t prefix" :: Named CaseInsensitive "CR123")
       `isPrefixOfName`
       ("This is a\n\t Prefix!" :: Named CaseInsensitive "CR123"))
      `shouldBe` True

    it "CR124 rejects an invalid prefix CaseInsensitive" $
      (("bad" :: Named CaseInsensitive "CR124")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitive "CR124"))
      `shouldBe` False

    it "CR125 rejects an too-long prefix CaseInsensitive" $
      (("stuffing" :: Named CaseInsensitive "CR125")
       `isPrefixOfName` ("stuff" :: Named CaseInsensitive "CR125"))
      `shouldBe` False

    it "CR126 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitive "CR126")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitive "CR126"))
      `shouldBe` True

    -----------------------------

    it "CR130 can check a null prefix of a null CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR130")
       `isPrefixOfName`
       ("" :: Named CaseInsensitivePreserve "CR130")) `shouldBe` True

    it "CR131 can check a null prefix CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR131")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR131"))
      `shouldBe` True

    it "CR132 can check a valid prefix CaseInsensitivePreserve" $
      (("sT" :: Named CaseInsensitivePreserve "CR132")
       `isPrefixOfName`
       ("Stuff" :: Named CaseInsensitivePreserve "CR132"))
      `shouldBe` True

    it "CR133 can check a long multi-word prefix CaseInsensitivePreserve" $
      (("This is A\n\t prefix" :: Named CaseInsensitivePreserve "CR133")
       `isPrefixOfName`
       ("This is a\n\t Prefix!" :: Named CaseInsensitivePreserve "CR133"))
      `shouldBe` True

    it "CR134 rejects an invalid prefix CaseInsensitivePreserve" $
      (("bad" :: Named CaseInsensitivePreserve "CR134")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR134"))
      `shouldBe` False

    it "CR135 rejects an too-long prefix CaseInsensitivePreserve" $
      (("stuffing" :: Named CaseInsensitivePreserve "CR135")
       `isPrefixOfName` ("stuff" :: Named CaseInsensitivePreserve "CR135"))
      `shouldBe` False

    it "CR136 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitivePreserve "CR136")
       `isPrefixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR136"))
      `shouldBe` True

    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for Secure.

    -- it "CR140 can check a null prefix of a null Secure" $
    --   (("" :: Named Secure "CR140")
    --    `isPrefixOfName`
    --    ("" :: Named Secure "CR140")) `shouldBe` True

    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for HTMLStyle.

    -- it "CR150 can check a null prefix of a null HTMLStyle" $
    --   (("" :: Named HTMLStyle "CR150")
    --    `isPrefixOfName`
    --    ("" :: Named HTMLStyle "CR150")) `shouldBe` True

  describe "Name suffix check" $ do

    it "CR210 can check a null suffix of a null UTF8" $
      (("" :: Name "CR210") `isSuffixOfName` ("" :: Name "CR210")) `shouldBe` True

    it "CR211 can check a null suffix UTF8" $
      (("" :: Name "CR211") `isSuffixOfName` ("stuff" :: Name "CR211"))
      `shouldBe` True

    it "CR212 can check a valid suffix UTF8" $
      (("uff" :: Name "CR212") `isSuffixOfName` ("stuff" :: Name "CR212"))
      `shouldBe` True

    it "CR213 can check a long multi-word suffix UTF8" $
      (("!" :: Name "CR213")
       `isSuffixOfName`
       ("This is a\n\t suffix!" :: Name "CR213")) `shouldBe` True

    it "CR214 rejects an invalid suffix UTF8" $
      (("bad" :: Name "CR214") `isSuffixOfName` ("stuff" :: Name "CR214"))
      `shouldBe` False

    it "CR215 rejects an too-long suffix UTF8" $
      (("stuffing" :: Name "CR215") `isSuffixOfName` ("stuff" :: Name "CR215"))
      `shouldBe` False

    it "CR216 rejects an case mismatch UTF8" $
      (("STUFF" :: Name "CR216") `isSuffixOfName` ("stuff" :: Name "CR216"))
      `shouldBe` False

    -----------------------------

    it "CR220 can check a null suffix of a null CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR220")
       `isSuffixOfName`
       ("" :: Named CaseInsensitive "CR220")) `shouldBe` True

    it "CR221 can check a null suffix CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR221")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitive "CR221"))
      `shouldBe` True

    it "CR222 can check a valid suffix CaseInsensitive" $
      (("Uff" :: Named CaseInsensitive "CR222")
       `isSuffixOfName`
       ("Stuff" :: Named CaseInsensitive "CR222"))
      `shouldBe` True

    it "CR223 can check a long multi-word suffix CaseInsensitive" $
      (("This is A\n\t suffix!" :: Named CaseInsensitive "CR223")
       `isSuffixOfName`
       ("This is a\n\t Suffix!" :: Named CaseInsensitive "CR223"))
      `shouldBe` True

    it "CR224 rejects an invalid suffix CaseInsensitive" $
      (("bad" :: Named CaseInsensitive "CR224")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitive "CR224"))
      `shouldBe` False

    it "CR225 rejects an too-long suffix CaseInsensitive" $
      (("stuffing" :: Named CaseInsensitive "CR225")
       `isSuffixOfName` ("stuff" :: Named CaseInsensitive "CR225"))
      `shouldBe` False

    it "CR226 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitive "CR226")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitive "CR226"))
      `shouldBe` True

    -----------------------------

    it "CR230 can check a null suffix of a null CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR230")
       `isSuffixOfName`
       ("" :: Named CaseInsensitivePreserve "CR230")) `shouldBe` True

    it "CR231 can check a null suffix CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR231")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR231"))
      `shouldBe` True

    it "CR232 can check a valid suffix CaseInsensitivePreserve" $
      (("uFf" :: Named CaseInsensitivePreserve "CR232")
       `isSuffixOfName`
       ("Stuff" :: Named CaseInsensitivePreserve "CR232"))
      `shouldBe` True

    it "CR233 can check a long multi-word suffix CaseInsensitivePreserve" $
      ((" A\n\t suffix!" :: Named CaseInsensitivePreserve "CR233")
       `isSuffixOfName`
       ("This is a\n\t Suffix!" :: Named CaseInsensitivePreserve "CR233"))
      `shouldBe` True

    it "CR234 rejects an invalid suffix CaseInsensitivePreserve" $
      (("bad" :: Named CaseInsensitivePreserve "CR234")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR234"))
      `shouldBe` False

    it "CR235 rejects an too-long suffix CaseInsensitivePreserve" $
      (("stuffing" :: Named CaseInsensitivePreserve "CR235")
       `isSuffixOfName` ("stuff" :: Named CaseInsensitivePreserve "CR235"))
      `shouldBe` False

    it "CR236 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitivePreserve "CR236")
       `isSuffixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR236"))
      `shouldBe` True

    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for Secure.

    -- it "CR240 can check a null suffix of a null Secure" $
    --   (("" :: Named Secure "CR240")
    --    `isSuffixOfName`
    --    ("" :: Named Secure "CR240")) `shouldBe` True

    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for HTMLStyle.

    -- it "CR250 can check a null suffix of a null HTMLStyle" $
    --   (("" :: Named HTMLStyle "CR250")
    --    `isSuffixOfName`
    --    ("" :: Named HTMLStyle "CR250")) `shouldBe` True

  describe "Name infix check" $ do

    it "CR310 can check a null infix of a null UTF8" $
      (("" :: Name "CR310") `isInfixOfName` ("" :: Name "CR310")) `shouldBe` True

    it "CR311 can check a null infix UTF8" $
      (("" :: Name "CR311") `isInfixOfName` ("stuff" :: Name "CR311"))
      `shouldBe` True

    it "CR312 can check a valid ending infix UTF8" $
      (("uff" :: Name "CR312") `isInfixOfName` ("stuff" :: Name "CR312"))
      `shouldBe` True

    it "CR313 can check a long multi-word infix UTF8" $
      (("!" :: Name "CR313")
       `isInfixOfName`
       ("This is a\n\t infix!" :: Name "CR313")) `shouldBe` True

    it "CR314 rejects an invalid infix UTF8" $
      (("bad" :: Name "CR314") `isInfixOfName` ("stuff" :: Name "CR314"))
      `shouldBe` False

    it "CR315 rejects an too-long infix UTF8" $
      (("stuffing" :: Name "CR315") `isInfixOfName` ("stuff" :: Name "CR315"))
      `shouldBe` False

    it "CR316 rejects an case mismatch UTF8" $
      (("STUFF" :: Name "CR316") `isInfixOfName` ("stuff" :: Name "CR316"))
      `shouldBe` False

    it "CR317 can check a valid internal infix UTF8" $
      (("tuf" :: Name "CR312") `isInfixOfName` ("stuff" :: Name "CR312"))
      `shouldBe` True

    it "CR318 can check a valid starting infix UTF8" $
      (("st" :: Name "CR318") `isInfixOfName` ("stuff" :: Name "CR318"))
      `shouldBe` True

    it "CR319 can check a valid equality is an infix UTF8" $
      (("stuff" :: Name "CR319") `isInfixOfName` ("stuff" :: Name "CR319"))
      `shouldBe` True

    -----------------------------

    it "CR320 can check a null infix of a null CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR320")
       `isInfixOfName`
       ("" :: Named CaseInsensitive "CR320")) `shouldBe` True

    it "CR321 can check a null infix CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR321")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitive "CR321"))
      `shouldBe` True

    it "CR322 can check a valid infix CaseInsensitive" $
      (("Uff" :: Named CaseInsensitive "CR322")
       `isInfixOfName`
       ("Stuff" :: Named CaseInsensitive "CR322"))
      `shouldBe` True

    it "CR323 can check a long multi-word infix CaseInsensitive" $
      (("s A\n\t i" :: Named CaseInsensitive "CR323")
       `isInfixOfName`
       ("This is a\n\t Infix!" :: Named CaseInsensitive "CR323"))
      `shouldBe` True

    it "CR324 rejects an invalid infix CaseInsensitive" $
      (("bad" :: Named CaseInsensitive "CR324")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitive "CR324"))
      `shouldBe` False

    it "CR325 rejects an too-long infix CaseInsensitive" $
      (("stuffing" :: Named CaseInsensitive "CR325")
       `isInfixOfName` ("stuff" :: Named CaseInsensitive "CR325"))
      `shouldBe` False

    it "CR326 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitive "CR326")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitive "CR326"))
      `shouldBe` True

    -----------------------------

    it "CR330 can check a null infix of a null CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR330")
       `isInfixOfName`
       ("" :: Named CaseInsensitivePreserve "CR330")) `shouldBe` True

    it "CR331 can check a null infix CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR331")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR331"))
      `shouldBe` True

    it "CR332 can check a valid infix CaseInsensitivePreserve" $
      (("uFf" :: Named CaseInsensitivePreserve "CR332")
       `isInfixOfName`
       ("Stuff" :: Named CaseInsensitivePreserve "CR332"))
      `shouldBe` True

    it "CR333 can check a long multi-word infix CaseInsensitivePreserve" $
      ((" A\n\t inf" :: Named CaseInsensitivePreserve "CR333")
       `isInfixOfName`
       ("This is a\n\t Infix!" :: Named CaseInsensitivePreserve "CR333"))
      `shouldBe` True

    it "CR334 rejects an invalid infix CaseInsensitivePreserve" $
      (("bad" :: Named CaseInsensitivePreserve "CR334")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR334"))
      `shouldBe` False

    it "CR335 rejects an too-long infix CaseInsensitivePreserve" $
      (("stuffing" :: Named CaseInsensitivePreserve "CR335")
       `isInfixOfName` ("stuff" :: Named CaseInsensitivePreserve "CR335"))
      `shouldBe` False

    it "CR336 accepts a case mismatch UTF8" $
      (("STUFF" :: Named CaseInsensitivePreserve "CR336")
       `isInfixOfName`
       ("stuff" :: Named CaseInsensitivePreserve "CR336"))
      `shouldBe` True

    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for Secure..

    -- it "CR340 can check a null infix of a null Secure" $
    --   (("" :: Named Secure "CR340")
    --    `isInfixOfName`
    --    ("" :: Named Secure "CR340")) `shouldBe` True


    -----------------------------
    -- n.b. These would cause a type error because NameUtilities are not defined
    -- for HTMLStyle.

    -- it "CR350 can check a null infix of a null HTMLStyle" $
    --   (("" :: Named HTMLStyle "CR350")
    --    `isInfixOfName`
    --    ("" :: Named HTMLStyle "CR350")) `shouldBe` True

  describe "Name take and drop" $ do

    let sampleInp = "take and drop" :: Name "t&d"

    it "CR360 can take nothing" $ do
      takeName 0 sampleInp `shouldBe` fromText ""

    it "CR361 can take everything" $ do
      takeName (nameLength sampleInp) sampleInp `shouldBe` sampleInp

    it "CR362 can take some" $ do
      takeName 3 sampleInp `shouldBe` "tak"

    it "CR370 can drop nothing" $ do
      dropName 0 sampleInp `shouldBe` sampleInp

    it "CR371 can drop everything" $ do
      dropName (nameLength sampleInp) sampleInp `shouldBe` fromText ""

    it "CR372 can drop some" $ do
      dropName 3 sampleInp `shouldBe` fromText "e and drop"

    it "CR373 can drop while nothing" $ do
      dropNameWhile (const False) sampleInp `shouldBe` sampleInp

    it "CR374 can drop while everything" $ do
      dropNameWhile (const True) sampleInp `shouldBe` ""

    it "CR375 can drop while some" $ do
      dropNameWhile (/= ' ') sampleInp `shouldBe` fromText " and drop"

    it "CR376 can drop while end nothing" $ do
      dropNameWhileEnd (const False) sampleInp `shouldBe` sampleInp

    it "CR377 can drop while end everything" $ do
      dropNameWhileEnd (const True) sampleInp `shouldBe` ""

    it "CR378 can drop while end some" $ do
      dropNameWhileEnd (/= ' ') sampleInp `shouldBe` fromText "take and "

    --------------------

    let sAmPLeInp = "TaKe aNd drOp" :: Named CaseInsensitive "t&d"

    it "CR380 can take nothing" $ do
      takeName 0 sAmPLeInp `shouldBe` fromText ""

    it "CR381 can take everything" $ do
      takeName (nameLength sAmPLeInp) sAmPLeInp `shouldBe` sAmPLeInp

    it "CR382 can take some" $ do
      takeName 3 sAmPLeInp `shouldBe` "taK"

    it "CR390 can drop nothing" $ do
      dropName 0 sAmPLeInp `shouldBe` sAmPLeInp

    it "CR391 can drop everything" $ do
      dropName (nameLength sAmPLeInp) sAmPLeInp `shouldBe` fromText ""

    it "CR392 can drop some" $ do
      dropName 3 sAmPLeInp `shouldBe` fromText "e AND DROP"

    it "CR393 can drop while nothing" $ do
      dropNameWhile (const False) sAmPLeInp `shouldBe` sAmPLeInp

    it "CR394 can drop while everything" $ do
      dropNameWhile (const True) sAmPLeInp `shouldBe` ""

    it "CR395 can drop while some" $ do
      dropNameWhile (/= ' ') sAmPLeInp `shouldBe` fromText " AND drop"

    it "CR396 can drop while end nothing" $ do
      dropNameWhileEnd (const False) sAmPLeInp `shouldBe` sAmPLeInp

    it "CR397 can drop while end everything" $ do
      dropNameWhileEnd (const True) sAmPLeInp `shouldBe` ""

    it "CR398 can drop while end some" $ do
      dropNameWhileEnd (/= ' ') sAmPLeInp `shouldBe` fromText "Take AND "

    --------------------

    let sAMPLEInp = "TaKe aNd drOp" :: Named CaseInsensitivePreserve "t&d"

    it "CR400 can take nothing" $ do
      takeName 0 sAMPLEInp `shouldBe` fromText ""

    it "CR401 can take everything" $ do
      takeName (nameLength sAMPLEInp) sAMPLEInp `shouldBe` sAMPLEInp

    it "CR402 can take some" $ do
      takeName 3 sAMPLEInp `shouldBe` "taK"

    it "CR403 can drop nothing" $ do
      dropName 0 sAMPLEInp `shouldBe` sAMPLEInp

    it "CR404 can drop everything" $ do
      dropName (nameLength sAMPLEInp) sAMPLEInp `shouldBe` fromText ""

    it "CR405 can drop some" $ do
      dropName 3 sAMPLEInp `shouldBe` fromText "e AND DROP"

    it "CR406 can drop while nothing" $ do
      dropNameWhile (const False) sAMPLEInp `shouldBe` sAMPLEInp

    it "CR407 can drop while everything" $ do
      dropNameWhile (const True) sAMPLEInp `shouldBe` ""

    it "CR408 can drop while some" $ do
      dropNameWhile (/= ' ') sAMPLEInp `shouldBe` fromText " AND drop"

    it "CR409 can drop while end nothing" $ do
      dropNameWhileEnd (const False) sAMPLEInp `shouldBe` sAMPLEInp

    it "CR409a can drop while end everything" $ do
      dropNameWhileEnd (const True) sAMPLEInp `shouldBe` ""

    it "CR409b can drop while end some" $ do
      dropNameWhileEnd (/= ' ') sAMPLEInp `shouldBe` fromText "Take AND "

    --------------------
    -- n.b. These would cause a type error because NameUtilities are not
    -- supported for Secure because that would leak the Secure name:

    -- it "CR363 cannot take from secure" $ do
    --   takeName 0 ("hidden" :: Named Secure "CR363")
    --     `shouldBe`
    --     ("disallowed" :: Named Secure "CR363")

    -- it "CR373 can drop from secure" $ do
    --   dropName 0 ("hidden" :: Named Secure "CR363")
    --     `shouldBe`
    --     ("disallowed" :: Named Secure "CR363")

  describe "Name breaking check" $ do

    it "CR410 can check a null breakOn of a null UTF8" $
      (("" :: Name "CR410") `breakOnName` ("" :: Name "CR410"))
      `shouldBe` ("", "")

    it "CR411 can check a null infix UTF8" $
      (("" :: Name "CR411") `breakOnName` ("stuff" :: Name "CR411"))
      `shouldBe` ("stuff", "")

    it "CR412 can check a valid ending infix UTF8" $
      (("uff" :: Name "CR412") `breakOnName` ("stuff" :: Name "CR412"))
      `shouldBe` ("st", "uff")

    it "CR413 can check a long multi-word infix UTF8" $
      (("!" :: Name "CR413")
       `breakOnName`
       ("This is a\n\t infix!" :: Name "CR413"))
      `shouldBe`
      ("This is a\n\t infix", "!")

    it "CR414 rejects an invalid infix UTF8" $
      (("bad" :: Name "CR414") `breakOnName` ("stuff" :: Name "CR414"))
      `shouldBe`
      ("stuff", "")

    it "CR415 rejects an too-long infix UTF8" $
      (("stuffing" :: Name "CR415") `breakOnName` ("stuff" :: Name "CR415"))
      `shouldBe`
      ("stuff", "")

    it "CR416 rejects an case mismatch UTF8" $
      (("STUFF" :: Name "CR416") `breakOnName` ("stuff" :: Name "CR416"))
      `shouldBe`
      ("stuff", "")

    it "CR417 can check a valid internal infix UTF8" $
      (("tuf" :: Name "CR412") `breakOnName` ("stuff" :: Name "CR412"))
      `shouldBe`
      ("s", "tuff")

    it "CR418 can check a valid starting infix UTF8" $
      (("st" :: Name "CR418") `breakOnName` ("stuff" :: Name "CR418"))
      `shouldBe`
      ("", "stuff")

    it "CR419 can check a valid equality is an infix UTF8" $
      (("stuff" :: Name "CR419") `breakOnName` ("stuff" :: Name "CR419"))
      `shouldBe`
      ("", "stuff")

    it "CR419a can break nothing" $ do
      breakName (const False) ("some stuff" :: Name "CR419a")
        `shouldBe`
         ("some stuff" :: Name "CR419a", "")

    it "CR419b can break everything" $ do
      breakName (const True) ("some stuff" :: Name "CR419b")
        `shouldBe` ("", "some stuff")

    it "CR419c can break some" $ do
      breakName (== ' ') ("some stuff" :: Name "CR419b")
        `shouldBe` (fromText "some", " stuff")

    --------------------

    it "CR420 can check a null breakOn of a null CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR420")
       `breakOnName`
       ("" :: Named CaseInsensitive "CR420"))
      `shouldBe` ("", "")

    it "CR421 can check a null infix CaseInsensitive" $
      (("" :: Named CaseInsensitive "CR421")
       `breakOnName`
       ("stuff" :: Named CaseInsensitive "CR421"))
      `shouldBe` ("stuff", "")

    it "CR422 can check a valid ending infix CaseInsensitive" $
      (("Uff" :: Named CaseInsensitive "CR422")
       `breakOnName`
       ("Stuff" :: Named CaseInsensitive "CR422"))
      `shouldBe` ("st", "uff")

    it "CR423 can check a long multi-word infix CaseInsensitive" $
      (("!" :: Named CaseInsensitive "CR423")
       `breakOnName`
       ("This is A\n\t infix!" :: Named CaseInsensitive "CR423"))
      `shouldBe`
      ("This is a\n\t INFIX", "!")

    it "CR424 rejects an invalid infix CaseInsensitive" $
      (("bad" :: Named CaseInsensitive "CR424")
       `breakOnName`
       ("stuff" :: Named CaseInsensitive "CR424"))
      `shouldBe`
      ("stuff", "")

    it "CR425 rejects an too-long infix CaseInsensitive" $
      (("stuffing" :: Named CaseInsensitive "CR425")
       `breakOnName`
       ("stuff" :: Named CaseInsensitive "CR425"))
      `shouldBe`
      ("stuff", "")

    it "CR426 accepts a case mismatch CaseInsensitive" $
      (("STUFF" :: Named CaseInsensitive "CR426")
       `breakOnName`
       ("stuff" :: Named CaseInsensitive "CR426"))
      `shouldBe`
      ("", "stuff")

    it "CR427 can check a valid internal infix CaseInsensitive" $
      (("tUf" :: Named CaseInsensitive "CR422")
       `breakOnName`
       ("STUFf" :: Named CaseInsensitive "CR422"))
      `shouldBe`
      ("s", "Tuff")

    it "CR428 can check a valid starting infix CaseInsensitive" $
      (("st" :: Named CaseInsensitive "CR428")
       `breakOnName`
       ("STUFF" :: Named CaseInsensitive "CR428"))
      `shouldBe`
      ("", "stUFf")

    --------------------

    it "CR430 can check a null breakOn of a null CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR430")
       `breakOnName`
       ("" :: Named CaseInsensitivePreserve "CR430"))
      `shouldBe` ("", "")

    it "CR431 can check a null infix CaseInsensitivePreserve" $
      (("" :: Named CaseInsensitivePreserve "CR431")
       `breakOnName`
       ("stuff" :: Named CaseInsensitivePreserve "CR431"))
      `shouldBe` ("stuff", "")

    it "CR432 can check a valid ending infix CaseInsensitivePreserve" $
      (("Uff" :: Named CaseInsensitivePreserve "CR432")
       `breakOnName`
       ("Stuff" :: Named CaseInsensitivePreserve "CR432"))
      `shouldBe` ("st", "uff")

    it "CR433 can check a long multi-word infix CaseInsensitivePreserve" $
      (("!" :: Named CaseInsensitivePreserve "CR433")
       `breakOnName`
       ("This is A\n\t infix!" :: Named CaseInsensitivePreserve "CR433"))
      `shouldBe`
      ("This is a\n\t INFIX", "!")

    it "CR434 rejects an invalid infix CaseInsensitivePreserve" $
      (("bad" :: Named CaseInsensitivePreserve "CR434")
       `breakOnName`
       ("stuff" :: Named CaseInsensitivePreserve "CR434"))
      `shouldBe`
      ("stuff", "")

    it "CR435 rejects an too-long infix CaseInsensitivePreserve" $
      (("stuffing" :: Named CaseInsensitivePreserve "CR435")
       `breakOnName`
       ("stuff" :: Named CaseInsensitivePreserve "CR435"))
      `shouldBe`
      ("stuff", "")

    it "CR436 accepts a case mismatch CaseInsensitivePreserve" $
      (("STUFF" :: Named CaseInsensitivePreserve "CR436")
       `breakOnName`
       ("stuff" :: Named CaseInsensitivePreserve "CR436"))
      `shouldBe`
      ("", "stuff")

    it "CR437 can check a valid internal infix CaseInsensitivePreserve" $
      (("tUf" :: Named CaseInsensitivePreserve "CR432")
       `breakOnName`
       ("STUFf" :: Named CaseInsensitivePreserve "CR432"))
      `shouldBe`
      ("s", "Tuff")

    it "CR438 can check a valid starting infix CaseInsensitivePreserve" $
      (("st" :: Named CaseInsensitivePreserve "CR438")
       `breakOnName`
       ("STUFF" :: Named CaseInsensitivePreserve "CR438"))
      `shouldBe`
      ("", "stUFf")


----------------------------------------------------------------------
-- Data.Name.JSON

#ifdef VERSION_aeson

instance TestShow (Proxy JSONStyle) where testShow _ = "Proxy :: \"JSONStyle\""

testJSON :: IO TestTree
testJSON = testSpec "Named JSON style" $ do
  describe "Named JSON" $ do

    it "CR90 can create a JSON Named from Text" $
      withChecklist "CR90" $
      fromText @(Named JSONStyle "CR90") ("test JSON text" :: Text)
      `checkValues`
      (Empty
       :> Val "overloaded equivalent text value" id "test JSON text"
       :> Got "case sensitive text value" (/= "Test json Text")
       :> Val "name parameter value" (\n -> nameOf n proxy#) "CR90"
       :> Val "style proxy" styleProxy (Proxy :: Proxy JSONStyle)
       :> Val "extracted text" nameText ("test JSON text" :: Text)
      )

    it "CR91 render UTF8 via Sayable" $
      withChecklist "CR91" $
      ("test JSON thing" :: Named JSONStyle "CR91")
      `checkValues`
      (Empty
       :> Val "as sayable" (sez @"test") "CR91 'test JSON thing'"
       :> Val "as sayable info" (sez @"info") "test JSON thing"
      )

    it "CR92 render UTF8 via Prettyprinter" $
      withChecklist "CR92" $
      fromText @(Named JSONStyle "CR92") ("test of JSON text" :: Text)
      `checkValues`
      (Empty
       :> Val "as pretty" (show . PP.pretty) "CR92 'test of JSON text'"
      )

    it "CR93 render UTF8 via Show" $
      withChecklist "CR93" $
      fromText @(Named JSONStyle "CR93") ("test of text" :: Text)
      `checkValues`
      (Empty
       :> Val "as show" show "CR93 'test of text'"
      )

    it "CR94 JSON semigroup" $
      withChecklist "CR94" $
      (fromText @(Named JSONStyle "CR94") ("more test JSON text" :: Text) <> "!!")
      `checkValues`
      (Empty
       :> Val "raw value" id "more test JSON text!!"
       :> Val "as sayable" (sez @"test") "CR94 'more test JSON text!!'"
      )

    it "CR95 JSON default conversion" $
      withChecklist "CR95" $
      (convertName (fromText "some JSON text" :: Named JSONStyle "CR95")
       :: Named JSONStyle "CR95 2")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "some JSON text"
       :> Val "as sayable" (sez @"test") "CR95 2 'some JSON text'"
       :> Val "as extracted text" nameText "some JSON text"
       :> Val "nameOf" (\n -> nameOf n proxy#) "CR95 2"
       :> Val "style proxy" styleProxy (Proxy :: Proxy JSONStyle)
      )

    it "CR96 UTF8->JSON default conversion" $
      withChecklist "CR96" $
      (convertStyle (fromText "Some JSON-able TEXT" :: Name "CR96")
        :: Named JSONStyle "CR96")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some JSON-able TEXT"
       :> Val "as sayable" (sez @"test") "CR96 'Some JSON-able TEXT'"
       :> Val "as extracted text" nameText "Some JSON-able TEXT"
      )

    it "CR97 JSON->UTF8 default conversion" $
      withChecklist "CR97" $
      (convertStyle (fromText "Some un-JSON-able TEXT" :: Named JSONStyle "CR97")
        :: Name "CR97")
      `checkValues`
      (Empty
       :> Val "matches implicit construction" id "Some un-JSON-able TEXT"
       :> Val "as sayable" (sez @"test") "CR97 'Some un-JSON-able TEXT'"
       :> Val "as extracted text" nameText "Some un-JSON-able TEXT"
      )

    it "CR98 can get a JSON length" $
      nameLength ("Length of JSON TEXT" :: Named JSONStyle "CR98") `shouldBe` 19

    it "CR99 can check a null JSON Named" $
      nullName ("" :: Named JSONStyle "CR99") `shouldBe` True

    it "CR100 can check a non-null JSON Named" $
      nullName ("*" :: Named JSONStyle "CR99") `shouldBe` False

    it "CR101 Named toJSON is valid" $
      withChecklist "CR101" $
      let obj = convertStyle ("Regular TEXT!" :: Name "CR101")
                :: Named JSONStyle "CR101"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id "\"Regular TEXT!\""
          :> Val "decoded" decode (Just obj)
         )

    it "CR102 toJSON of structure containing Named is valid" $
      withChecklist "CR102" $
      let obj = Foo "start" "your engines"
                (Map.fromList [ ("one","ready")
                              , ("two", "set")
                              , ("three", "go")
                              ])
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "[\"start\",\"your engines\",{\"one\":\"ready\",\"three\":\"go\",\"two\":\"set\"}]"
          :> Val "decoded" decode (Just obj)
         )

    it "CR103 toJSON of record containing Named is valid" $
      withChecklist "CR103" $
      let obj = Info "John Henry" "railroad worker" "hammer master"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "{\"desc\":\"hammer master\",\"name\":\"John Henry\",\"title\":\"railroad worker\"}"
          :> Val "decoded" decode (Just obj)
         )

    it "CR104 JSON of record containing CaseInsensitive Named is not necessarily round-robin but CaseInsensitivePreserve is" $
      withChecklist "CR104" $
      let obj = Info "John Henry" "Railroad Worker" "Hammer Master"
      in encode (toJSON obj)
         `checkValues`
         (Empty
          :> Val "encoded" id
          "{\"desc\":\"Hammer Master\",\"name\":\"John Henry\",\"title\":\"railroad worker\"}"
          :> Val "decoded" decode (Just $ obj { title = fromText $ T.toLower $ nameText $ title obj })
         )


data Info = Info { name :: Named CaseInsensitivePreserve "name"
                 , title :: Named CaseInsensitive "title"
                 , desc :: Name "description"
                 }
  deriving (Eq, Generic, Show)
instance ToJSON Info
instance FromJSON Info

instance ToJSON Foo
instance FromJSON Foo

instance ConvertName JSONStyle "CR95" "CR95 2"

instance TestShow ByteString
instance TestShow a => TestShow (Maybe a) where
  testShow = \case
    Nothing -> "NOTHING"
    Just a -> "JUST " <> testShow a
instance TestShow Info
#endif
