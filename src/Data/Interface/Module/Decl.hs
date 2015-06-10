{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Interface.Module.Decl
 (
    Decl(..)
  , ValueDecl
  , TypeDecl
  , SomeDecl(..)
  , DeclInfo(..)
  , Type
  , Kind
  , rawDecl
 )
where

import Data.Interface.Name


-- | A top-level declaration with an identifier in namespace @s@.
data Decl s = Decl !(Name s) (DeclInfo s)
    deriving (Show, Eq, Ord)

-- | Construct a @Decl s@ with the given `RawName`, lifted into the namespace
-- @s@ required by the given @DeclInfo s@.
rawDecl :: RawName -> DeclInfo s -> Decl s
rawDecl s info = case info of
    Value{}      -> Decl (ValueName s) info
    PatternSyn{} -> Decl (ValueName s) info
    DataCon{}    -> Decl (ValueName s) info
    DataType{}   -> Decl (TypeName s) info
    TypeSyn{}    -> Decl (TypeName s) info
    TypeClass{}  -> Decl (TypeName s) info


type ValueDecl = Decl 'Values
type TypeDecl = Decl 'Types

data SomeDecl where
    SomeDecl :: !(Decl s) -> SomeDecl

deriving instance Show SomeDecl


instance HasNamespace (Decl s) where
    type Space (Decl s) = 'Just s

    namespace (Decl name _) = namespace name

instance HasNamespace SomeDecl where
    type Space SomeDecl = 'Nothing

    namespace (SomeDecl decl) = namespace decl

instance HasRawName (Decl s) where
    rawName (Decl name _) = rawName name


type Type = String      -- ^ TODO
type Kind = [String]    -- ^ TODO

-- | The content of a top-level declaration, without the identifier
data DeclInfo (s :: Namespace) where
    Value :: String -> DeclInfo 'Values
        -- ^ top-level value/identifier
    PatternSyn :: Type -> DeclInfo 'Values
        -- ^ a pattern synonym
    DataCon :: Type -> DeclInfo 'Values
        -- ^ a data constructor
    DataType :: Kind -> DeclInfo 'Types
        -- ^ a newtype/data declaration
    TypeSyn :: Kind -> String -> DeclInfo 'Types
        -- ^ a type synonym w/ a kind and definition
    TypeClass :: Kind -> DeclInfo 'Types
        -- ^ a typeclass

deriving instance Show (DeclInfo s)
deriving instance Eq (DeclInfo s)
deriving instance Ord (DeclInfo s)

{- DeclInfo notes:
      - Each of these constructors will later become distinct types
      - TypeSyn contains its definition, because this affects its interface
            (this is only a String for now, but will have to include
             first-class type information)
      - Type constructors will need a list of their data constructors
      - The distinction between data constructors and other values might
        be problematic

      TODO:
        - type/data families
-}

instance HasNamespace (DeclInfo s) where
    type Space (DeclInfo s) = 'Just s

    namespace info = case info of
        Value{}      -> Values
        PatternSyn{} -> Values
        DataCon{}    -> Values
        DataType{}   -> Types
        TypeSyn{}    -> Types
        TypeClass{}  -> Types
