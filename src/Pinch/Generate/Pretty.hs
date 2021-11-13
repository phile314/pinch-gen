{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate.Pretty where

import           Data.String
import qualified Data.Text                 as T
import           Prelude hiding (mod)
import           Prettyprinter

newtype ModuleName = ModuleName T.Text
  deriving (Show)
type TypeName = T.Text
type Name = T.Text
type ClassName = T.Text

data Module = Module
  { modName    :: ModuleName
  , modPragmas :: [Pragma]
  , modImports :: [ImportDecl]
  , modDecls   :: [Decl]
  }
  deriving (Show)

data Pragma
  = PragmaLanguage T.Text
  | PragmaOptsGhc T.Text
  deriving (Show)

data ImportDecl = ImportDecl
  { iName      :: ModuleName
  , iQualified :: Bool
  , iThings    :: ImportNames
  }
  deriving (Show)

data ImportNames
  = IEverything
  | IJust [ Name ]
  deriving (Show)

data Decl
  = TypeDecl Type Type
  | DataDecl TypeName [ConDecl] [Deriving]
  | InstDecl InstHead [Decl]
  | FunBind [Match]
  | TypeSigDecl Name Type
  deriving (Show)

data Deriving
  = DeriveClass Type
  deriving (Show)

data ConDecl
  = ConDecl Name [Type]
  | RecConDecl Name [(Name, Type)]
  deriving (Show)

data Type
  = TyApp Type [Type]
  | TyCon TypeName
  | TyLam [Type] Type
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
  | EList [Exp]
  | ELam [Pat] Exp
  | ETuple [Exp]
  | ELet Name Exp Exp
  | ETyApp Exp [Type]
  deriving (Show)

data Stm
  = StmBind (Maybe Pat) Exp
  deriving (Show)

data Alt
  = Alt Pat Exp
  deriving (Show)

data Lit
  = LInt Integer
  | LFloat Double
  | LString T.Text
  deriving (Show)

instance Pretty ModuleName where
  pretty (ModuleName x) = pretty x

instance Pretty Module where
  pretty mod =
       vsep (map pretty $ modPragmas mod) <> line <> line
    <> "module" <+> pretty (modName mod) <+> "where" <> line <> line
    <> vsep (map pretty $ modImports mod) <> line <> line
    <> vsep (map pretty $ modDecls mod)

instance Pretty Pragma where
  pretty p = case p of
    PragmaLanguage p' -> "{-# LANGUAGE" <+> pretty p' <+> "#-}"
    PragmaOptsGhc o -> "{-# OPTIONS_GHC" <+> pretty o <+> "#-}"

instance Pretty ImportDecl where
  pretty i = "import" <+> (if (iQualified i) then "qualified" else "") <+> pretty (iName i) <> pretty (iThings i)

instance Pretty ImportNames where
  pretty i = case i of
    IEverything -> ""
    IJust xs -> " " <> (parens $ cList $ map pretty xs)

instance Pretty Decl where
  pretty decl = case decl of
    TypeDecl t1 t2 -> "type" <+> pretty t1 <+> "=" <+> pretty t2 <> line
    DataDecl t [] ds -> "data" <+> pretty t <+> prettyDerivings ds <> line
    DataDecl t (c:cs) ds -> nest 2 (vsep $
        [ "data" <+> pretty t
        , "=" <+> pretty c
        ] ++ (map (\c' -> "|" <+> pretty c') cs) ++ [ prettyDerivings ds ]
      ) <> line
    InstDecl h decls -> (nest 2 $ vsep $ [ pretty h ] ++ map pretty decls) <> line
    FunBind ms -> vsep (map pretty ms) <> line
    TypeSigDecl n ty -> pretty n <+> "::" <+> pretty ty

prettyDerivings :: [Deriving] -> Doc a
prettyDerivings [] = ""
prettyDerivings ds = "deriving" <+> (parens $ cList $ map pretty ds)

instance Pretty Deriving where
  pretty (DeriveClass c) = pretty c

instance Pretty ConDecl where
  pretty (ConDecl n args) = hsep $ [ pretty n ] ++ map pretty args
  pretty (RecConDecl n args) = hsep $ [ pretty n, "{", fields, "}" ]
    where fields = cList $ map (\(f, v) -> pretty f <+> "::" <+> pretty v) args

instance Pretty InstHead where
  pretty (InstHead cs n ty) = "instance" <> context <+> pretty n <+> pretty ty <+> "where"
    where context = if null cs then "" else space <> parens (cList $ map pretty cs) <+> "=>" <+> pretty n <+> pretty ty <+> "where"

instance Pretty Constraint where
  pretty (CClass cl n) = pretty cl <+> pretty n

instance Pretty Type where
  pretty ty = case ty of
    TyApp t1 ts -> parens $ pretty t1 <+> hsep (map pretty ts)
    TyCon t -> pretty t
    TyLam ts t -> concatWith (surround (space <> "->" <> space)) (map (parens . pretty) ts ++ [pretty t])

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
    EApp e' es -> pretty e' <+> hsep (map (parens . pretty) es)
    ELit l -> pretty l
    ETyAnn e' ty -> parens $ pretty e' <+> "::" <+> pretty ty
    ECase e' as -> nest 2 $ vsep $ ["case" <+> pretty e' <+> "of"] ++ map pretty as
    EDo s -> nest 2 $ vsep $ ["do"] ++ map pretty s
    EInfix op e1 e2 -> parens $ hsep [ pretty e1, pretty op, pretty e2]
    EList es -> "[" <+> cList (map pretty es) <+> "]"
    ELam ps e' -> parens $ "\\" <> hsep (map pretty ps) <+> "->" <+> pretty e'
    ETuple es -> nest 2 $ tupled $ map pretty es
    ELet nm e1 e2 -> "let" <+> pretty nm <+> "=" <+> indent 2 (pretty e1) <+> "in" <+> pretty e2
    ETyApp e' tys -> pretty e' <+>  hsep (map (("@"<>) . parens . pretty) tys)

instance Pretty Alt where
  pretty (Alt p e) = pretty p <+> "->" <+> pretty e

instance Pretty Stm where
  pretty s = case s of
    StmBind Nothing e -> pretty e
    StmBind (Just p) e -> pretty p <+> "<-" <+> pretty e

instance Pretty Lit where
  pretty l = case l of
    LInt i -> pretty i
    LFloat f -> pretty f
    LString t -> "\"" <> pretty t <> "\""

cList :: [Doc ann] -> Doc ann
cList = concatWith (surround (comma <> space))


instance IsString Exp where
  fromString = EVar . T.pack

instance IsString Pat where
  fromString = PVar . T.pack
