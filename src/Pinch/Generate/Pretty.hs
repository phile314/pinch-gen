{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate.Pretty where

import Data.String
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

newtype ModuleName = ModuleName T.Text
  deriving (Show)
newtype TypeName = TypeName T.Text
  deriving (Show)
newtype Name = Name T.Text
  deriving (Show)
newtype ClassName = ClassName T.Text
  deriving (Show)

data Module = Module
  { modName :: ModuleName
  , modPragmas :: [Pragma]
  , modImports :: [ImportDecl]
  , modDecls :: [Decl]
  }
  deriving (Show)

data Pragma =
  PragmaLanguage T.Text
  deriving (Show)

data ImportDecl = ImportDecl
  { iName :: ModuleName
  , iQualified :: Bool
  }
  deriving (Show)

data Decl
  = TypeDecl Type Type
  | DataDecl TypeName [ConDecl]
  | InstDecl InstHead [Decl]
  | FunBind [Match]
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  | RecConDecl Name [(Name, Type)]
  deriving (Show)

data Type
  = TyApp Type [Type]
  | TyCon TypeName
  deriving (Show)


data InstHead
  = InstHead [Constraint] ClassName Type
  deriving (Show)

data Constraint
  = CClass ClassName Type
  deriving (Show)

data Match = Match Name [Pat] Exp
  deriving (Show)

data Pat
  = PVar Name
  | PLit Lit
  | PCon Name [Pat]
  deriving (Show)

data Exp
  = EVar Name
  | EApp Exp [Exp]
  | ELit Lit
  | ETyAnn Exp Type
  | ECase Exp [Alt]
  | EDo [Stm]
  | EInfix Name Exp Exp
  deriving (Show)

data Stm
  = StmBind (Maybe Pat) Exp
  deriving (Show)

data Alt
  = Alt Pat Exp
  deriving (Show)

data Lit
  = LInt Integer
  | LString T.Text
  deriving (Show)

instance Pretty ModuleName where
  pretty (ModuleName x) = pretty x

instance Pretty TypeName where
  pretty (TypeName x) = pretty x

instance Pretty Name where
  pretty (Name x) = pretty x

instance Pretty ClassName where
  pretty (ClassName x) = pretty x

instance Pretty Module where
  pretty mod = vsep
    (map pretty (modPragmas mod)
     ++ [  "module" <+> pretty (modName mod) <+> "where"]
     ++ map pretty (modImports mod)
     ++ map pretty (modDecls mod))

instance Pretty Pragma where
  pretty p = case p of
    PragmaLanguage p -> "{-# LANGUAGE" <+> pretty p <+> "#-}"

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
    InstDecl h decls -> nest 2 $ vsep $ [ pretty h ] ++ map pretty decls
    FunBind ms -> vsep $ map pretty ms

instance Pretty ConDecl where
  pretty (ConDecl n args) = hsep $ [ pretty n ] ++ map pretty args
  pretty (RecConDecl n args) = hsep $ [ pretty n, "{", fields, "}" ]
    where fields = cList $ map (\(n, v) -> pretty n <+> "::" <+> pretty v) args

instance Pretty InstHead where
  pretty (InstHead cs n ty) = "instance" <> context <+> pretty n <+> pretty ty <+> "where"
    where context = if null cs then "" else space <> parens (cList $ map pretty cs) <+> "=>" <+> pretty n <+> pretty ty <+> "where"

instance Pretty Constraint where
  pretty (CClass cl n) = pretty cl <+> pretty n

instance Pretty Type where
  pretty ty = case ty of
    TyApp t1 ts -> pretty t1 <+> hsep (map pretty ts)
    TyCon t -> pretty t

instance Pretty Match where
  pretty (Match n ps e) = pretty n <+> hsep (map pretty ps) <+> "=" <+> pretty e

instance Pretty Pat where
  pretty p = case p of
    (PVar x) -> pretty x
    (PLit i) -> pretty i
    (PCon n []) -> pretty n
    (PCon n xs) -> parens $ pretty n <+> hsep (map pretty xs)

instance Pretty Exp where
  pretty e = case e of
    EVar n -> pretty n
    EApp e es -> pretty e <+> hsep (map (parens . pretty) es)
    ELit l -> pretty l
    ETyAnn e ty -> parens $ pretty e <+> "::" <+> pretty ty
    ECase e as -> nest 2 $ vsep $ ["case" <+> pretty e <+> "of"] ++ map pretty as
    EDo s -> nest 2 $ vsep $ ["do"] ++ map pretty s
    EInfix op e1 e2 -> hsep [ pretty e1, pretty op, pretty e2]

instance Pretty Alt where
  pretty (Alt p e) = pretty p <+> "->" <+> pretty e

instance Pretty Stm where
  pretty s = case s of
    StmBind Nothing e -> pretty e
    StmBind (Just p) e -> pretty p <+> "<-" <+> pretty e

instance Pretty Lit where
  pretty l = case l of
    LInt i -> pretty i
    LString t -> "\"" <> pretty t <> "\""

cList = concatWith (surround (comma <> space))


instance IsString Exp where
  fromString = EVar . Name . T.pack
