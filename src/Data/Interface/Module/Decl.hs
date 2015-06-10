{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{-| Types for top-level declarations.
-}
module Data.Interface.Module.Decl
 (
    Decl(..)
  , ValueDecl
  , TypeDecl
  , SomeDecl(..)
  , someDecl
  , DeclInfo(..)
  , Type
  , Kind
  , rawDecl
 )
where

import Data.Interface.Name


-- | A top-level declaration with an identifier in namespace @s@.
data Decl s = Decl
    { declName :: !(Name s)
    , declInfo :: DeclInfo s
    } deriving (Show, Eq, Ord)

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

-- | A @Decl s@ where @s@ is dynamically determined to be the value namespace
-- or the type namespace. Enables @s@ to be recovered through pattern-matching.
data SomeDecl where
    ValueDecl :: !(Decl 'Values) -> SomeDecl
    TypeDecl  :: !(Decl 'Types) -> SomeDecl

someDecl :: Decl s -> SomeDecl
someDecl decl = case declInfo decl of
    Value{}      -> ValueDecl decl
    PatternSyn{} -> ValueDecl decl
    DataCon{}    -> ValueDecl decl
    DataType{}   -> TypeDecl decl
    TypeSyn{}    -> TypeDecl decl
    TypeClass{}  -> TypeDecl decl


deriving instance Show SomeDecl


instance HasNamespace (Decl s) where
    type Space (Decl s) = 'Just s

    namespace (Decl name _) = namespace name

instance HasNamespace SomeDecl where
    type Space SomeDecl = 'Nothing

    namespace ValueDecl{} = Values
    namespace TypeDecl{}  = Types

instance HasRawName (Decl s) where
    rawName (Decl name _) = rawName name


type Type = String      -- ^ TODO
type Kind = [String]    -- ^ TODO

-- | The content of a top-level declaration, without the identifier
data DeclInfo (s :: Namespace) where
    -- a top-level value/identifier
    Value :: String -> DeclInfo 'Values
    -- a pattern synonym
    PatternSyn :: Type -> DeclInfo 'Values
    -- a data constructor
    DataCon :: Type -> DeclInfo 'Values
    -- a newtype/data declaration
    DataType :: Kind -> DeclInfo 'Types
    -- a type synonym w/ a kind and definition
    TypeSyn :: Kind -> String -> DeclInfo 'Types
    -- a typeclass
    TypeClass :: Kind -> DeclInfo 'Types

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
