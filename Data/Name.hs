{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Name
  ( type Name
  , Named
  , name, nameOf, nameProxy, styleProxy, caselessName
  , NameStyle, UTF8, CaseInsensitive, Secure
  , HasName, myName
  , NameText, nameText
  , SomeName(SomeName), viewSomeName
  , SomeNameStyle(SomeNameStyle), viewSomeNameStyle
  , SecureName, secureName, secureNameBypass
  , IsText(fromText)
  , ConvertName(convertName)
  , ConvertNameStyle(convertStyle)
  , ValidNames, validName
)
where

import           Control.DeepSeq ( NFData )
import           Data.Hashable ( Hashable )
import           Data.Proxy ( Proxy(Proxy) )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text )
import qualified Data.Text as T
import           GHC.Exts ( Proxy#, proxy#, IsList(fromList, toList), Item )
import           GHC.Generics ( Generic )
import           GHC.TypeLits
import           Prettyprinter ( (<+>) )
import qualified Prettyprinter as PP
import           Text.Sayable


-- | The 'Named' is a wrapper around any 'Text' that identifies the type of
-- 'Text' via the @sym@ phantom symbol type, as well as a usage specified by the
-- @style@ type parameter.  Use of 'Named' should always be preferred to using
-- a raw 'Text' (or 'String').

newtype Named (style :: NameStyle) (sym :: Symbol) =
  Named { named :: Text }
  deriving (Eq, Ord, Generic, NFData, Semigroup)

-- | The NameStyle specifies how the name itself is styled.
--
--  * The 'UTF8' default style is orthogonal to a normal String or Text.
--
--  * The 'CaseInsensitive' style indicates that uppercase ASCII characters are
--    equivalent to their lowercase form.
--
--  * The Secure style is case sensitive, but does not reveal the full contents
--    unless the specific "secureName" accessor function is used.  This is useful
--    for storing secrets (e.g. passphrases, access tokens, etc.) that should not
--    be fully visible in log messages and other miscellaneous output.

type NameStyle = Symbol
type UTF8 = "UTF8" :: NameStyle
type CaseInsensitive = "CaseInsensitive" :: NameStyle
type Secure = "SECURE!" :: NameStyle

instance Hashable (Named style sym)


-- | Retrieve the @sym@ type parameter (the "what am I") of a Named as a text
-- value
nameOf :: KnownSymbol sym => Named style sym -> Proxy# sym -> String
nameOf _ = symbolVal'


nameProxy :: KnownSymbol sym => Named style sym -> Proxy sym
nameProxy _ = Proxy

styleProxy :: KnownSymbol style => Named style sym -> Proxy style
styleProxy _ = Proxy


instance {-# OVERLAPPABLE #-} IsString (Named style sym) where
  fromString = Named . fromString


class IsText a where fromText :: Text -> a
instance {-# OVERLAPPABLE #-} IsText (Named style sym) where
  fromText = Named


class ConvertName style origTy newTy where
  convertName :: Named style origTy -> Named style newTy
  convertName = fromText . named

class ConvertNameStyle inpStyle outStyle nameTy where
  convertStyle :: Named inpStyle nameTy -> Named outStyle nameTy
  convertStyle = fromText . named


-- | For an @"info"@ 'saytag' (and possibly others), a 'Name' doesn't include its
-- label.  Normally, it shows the label followed by the text itself.  The Sayable
-- defers to the Prettyprinting instance for actual representation.
instance NameText style => Sayable "info" (Named style nm) where
  sayable = Saying . PP.pretty . nameText
instance {-# OVERLAPPABLE #-} (PP.Pretty (Named style nm)
                              ) => Sayable tag (Named style nm)
 where sayable = Saying . PP.pretty



----------------------------------------------------------------------

-- | The Name type is for the standard/most commonly used style which is
-- orthogonal to a normal String or Text.

type Name = Named UTF8

name :: Name sym -> Text
name = named


instance IsList (Name s) where
  type Item (Name s) = Item Text
  fromList = fromText . fromList
  toList = toList . name

instance ConvertName UTF8 a a where convertName = id
instance ConvertName UTF8 "component" "instance component"
instance ConvertName UTF8 "git.branch" "git.branch|ref"
instance ConvertName UTF8 "git.ref" "git.branch|ref"

instance KnownSymbol ty => PP.Pretty (Name ty) where
  pretty nm = (PP.pretty $ nameOf nm proxy#) <+> PP.squotes (PP.pretty (name nm))


----------------------------------------------------------------------

data SomeName =
  forall (s :: Symbol) . KnownSymbol s => SomeName (Name s)

viewSomeName :: (forall (s :: Symbol) . KnownSymbol s => Name s -> r) -> SomeName -> r
viewSomeName f (SomeName n) = f n


data SomeNameStyle nameTy =
  forall (s :: Symbol)
  . (KnownSymbol s, NameText s)
  => SomeNameStyle (Named s nameTy)

viewSomeNameStyle :: (forall (s :: Symbol) . KnownSymbol s => Named s nameTy -> r)
                  -> SomeNameStyle nameTy -> r
viewSomeNameStyle f (SomeNameStyle n) = f n


----------------------------------------------------------------------

-- | Some objects have (contain) an associated name.  If they do, they can
-- declare the HasName constraint, and use myName to reconsistute the name from
-- the object.

class HasName x style nm | x -> style, x -> nm where
  myName :: x -> Named style nm


-- | A general class that can be used to extract the Text back out of a name.

class NameText style where
  nameText :: Named style nm -> Text
  nameText = named

instance NameText UTF8


----------------------------------------------------------------------

instance {-# OVERLAPPING #-} IsString (Named CaseInsensitive sym) where
  fromString = Named . T.toLower . fromString

instance {-# OVERLAPPING #-} IsText (Named CaseInsensitive sym) where
  fromText = Named . T.toLower

instance KnownSymbol ty => PP.Pretty (Named CaseInsensitive ty) where
  pretty nm = (PP.pretty $ nameOf nm proxy#)
              <+> PP.surround (PP.pretty (caselessName nm)) "«" "»"

instance NameText CaseInsensitive where
  nameText = named

caselessName :: Named CaseInsensitive sym -> Text
caselessName = named


----------------------------------------------------------------------

-- | The SecureName is like Name, but its display form does not reveal
-- the full name.

type SecureName = Named Secure

-- | The secureName accessor is used to obtain the name field from a Secure
-- Named.  This is the normal accessor for a Secure Named and will occlude a
-- portion of the extracted name for protection.  For those specific cases where
-- the full Secure Named text is needed, the 'secureNameBypass' accessor should
-- be used instead.

secureName :: Named Secure sym -> Text
secureName nm = if T.length (named nm) < 5
                then T.replicate 8 "#"
                else ((T.take 2 $ named nm)
                      <> T.replicate (T.length (named nm) - 4) "#"
                      <> T.reverse (T.take 2 $ T.reverse $ named nm))

-- | The secureNameBypass accessor is used to obtain the raw Text from a Secure
-- Named; this essentially bypasses the security protection and should only be
-- used in the limited cases when the raw form is absolutely needed.

secureNameBypass :: Named Secure sym -> Text
secureNameBypass = named

-- instance NameText Secure sym   <-- explicitly not defined!

instance KnownSymbol ty => PP.Pretty (Named Secure ty) where
  pretty nm = (PP.pretty $ nameOf nm proxy#)
              <+> PP.squotes (PP.pretty (named nm))

-- Note that there should be no instance of ToJSON for a SecureName!


----------------------------------------------------------------------
-- | The ValidNames constraint can be used to specify the list of allowed names
-- for a parameterized name argument.


class ( KnownNat (AllowedNameType nty ntl)
      , DisallowedNameType nty ntl ntl
      )
  => ValidNames (nty :: Symbol) (ntl :: [Symbol]) where
  validName :: Proxy ntl -> Name nty -> Text


----------------------------------------------------------------------
-- Internal definitions to support the ValidNames constraint class
-- implementation.

instance ( KnownNat (AllowedNameType nty ntl)  -- n.b. if this fails, see Note-1
         , DisallowedNameType nty ntl ntl
         )
   => ValidNames nty ntl where
  validName _ = name

type family AllowedNameType (nty :: Symbol) (ntl :: [Symbol]) :: Nat where
  AllowedNameType nty (nty ': ntl) = 0
  AllowedNameType nty (any ': ntl) = 1 + (AllowedNameType nty ntl)

-- * Note-1
--
-- Normally the DisallowedNameType will generate a useful TypeError if
-- a parameter uses a name not in the allowed names list.  For example:
--
-- > foo :: ValidNames n '[ "name1", "name 2" ] -> Name n -> Bool
-- > foo (Name @"name1" "indiana")  -- OK
-- > foo (Name @"last" "jones")  -- generates TypeError indicating
--                               -- "last" is not a member of the
--                               -- allowed list ["name1", "name 2"]
--
-- However, when parametric constraints are cascaded and the
-- parametric constraints don't align, the compilation complaint will
-- be that there's no KnownNat for the AllowedNameType instance
-- constraint on ValidNames above.  for example:
--
-- > bar :: ValidNames n '[ "name1", "stage" ]
-- > bar n = foo n
--
-- generates the KnownNat error.  Not sure of a better way to handle
-- this at the moment.

class DisallowedNameType (nty :: Symbol) (okntl :: [Symbol]) (ntl :: [Symbol])

instance TypeError ('Text "Name '" ':<>: 'ShowType nty
                    ':<>: 'Text "' not in allowed Names: " ':<>: 'ShowType ntl)
         => DisallowedNameType nty '[] ntl
instance DisallowedNameType nty (nty ': ntys) ntl
instance {-# OVERLAPPABLE #-} DisallowedNameType nty ntys ntl
         => DisallowedNameType nty (oty ': ntys) ntl
