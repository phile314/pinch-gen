{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate.Pretty where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

newtype ModuleName = ModuleName T.Text
  deriving (Show)
newtype TypeName = TypeName T.Text
  deriving (Show)
newtype Name = Name T.Text
  deriving (Show)

data Module = Module
  { modName :: ModuleName
  , modImports :: [ImportDecl]
  , modDecls :: [Decl]
  }
  deriving (Show)

data ImportDecl = ImportDecl
  { iName :: ModuleName
  , iQualified :: Bool
  }
  deriving (Show)

data Decl
  = TypeDecl TypeName Type
  | DataDecl TypeName [ConDecl]
  | InstanceDecl
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  | RecConDecl Name [(Name, Type)]
  deriving (Show)

data Type
  = TyApp Type [Type]
  | TyCon TypeName
  deriving (Show)


instance Pretty ModuleName where
  pretty (ModuleName x) = pretty x

instance Pretty TypeName where
  pretty (TypeName x) = pretty x

instance Pretty Name where
  pretty (Name x) = pretty x

instance Pretty Module where
  pretty mod = vsep
    ([  "module" <+> pretty (modName mod) <+> "where"]
      ++ map pretty (modImports mod)
      ++ map pretty (modDecls mod))

instance Pretty ImportDecl where
  pretty i = "import" <+> (if (iQualified i) then "qualified" else "") <+> pretty (iName i)

instance Pretty Decl where
  pretty decl = case decl of
    TypeDecl t1 t2 -> "type" <+> pretty t1 <+> "=" <+> pretty t2
    DataDecl t [] -> "data" <+> pretty t
    DataDecl t (c:cs) -> nest 2 $ vsep $
      [ "data" <+> pretty t
      , "=" <+> pretty c
      ] ++ (map (\c -> "|" <+> pretty c) cs)

instance Pretty ConDecl where
  pretty (ConDecl n args) = hsep $ [ pretty n ] ++ map pretty args
  pretty (RecConDecl n args) = hsep $ [ pretty n, "{", fields, "}" ]
    where fields = concatWith (surround (comma <> space)) $ map (\(n, v) -> pretty n <+> "::" <+> pretty v) args


instance Pretty Type where
  pretty ty = case ty of
    TyApp t1 ts -> pretty t1 <+> hsep (map pretty ts)
    TyCon t -> pretty t
