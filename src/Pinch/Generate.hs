{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate where

import Language.Thrift.AST as A
import Language.Thrift.Parser
import Text.Megaparsec (SourcePos)
import qualified Text.Megaparsec.Error as E
import Control.Exception
import Data.Void
import Data.List
import System.FilePath
import qualified Data.Text as T
import qualified Pinch.Generate.Pretty as H
import Data.Char
import Data.Maybe
import qualified Data.HashMap.Strict as Map
import Control.Monad.Reader
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

generate :: FilePath -> FilePath -> IO ()
generate inp out = do
  thrift <- loadFile inp

  mod <- gProgram inp thrift

  putDoc $ pretty mod

extractModuleName :: FilePath -> T.Text
extractModuleName f = T.pack $ toUpper m : ms
  where
    (m:ms) = dropExtension $ takeFileName f

extractNamespace :: [Header SourcePos] -> Maybe T.Text
extractNamespace headers =
  listToMaybe $ mapMaybe (\x -> case x of
    HeaderNamespace (Namespace l n _) | l == "hs" || l == "*" -> Just (n <> ".")
    _ -> Nothing
  ) headers


loadFile :: FilePath -> IO (Program SourcePos)
loadFile inp = do
  thrift <- parseFromFile inp
  case thrift of
    Left err -> do
      putStrLn "Could not parse thrift file."
--      putStrLn $ E.errorBundlePretty $ err
      throwIO $ err
    Right s -> pure s



gProgram :: FilePath -> Program SourcePos -> IO H.Module
gProgram inp (Program headers defs) = do
  (imports, maps) <- unzip <$> traverse (gInclude baseDir) incHeaders

  let map = Map.unions maps
  pure $
    H.Module (H.ModuleName $ ns <> extractModuleName inp)
    imports
    (concat $ runReader (traverse gDefinition defs) map)

  where
    ns = fromMaybe "" $ extractNamespace headers
    baseDir = dropFileName inp
    incHeaders = mapMaybe (\x -> case x of
      HeaderInclude i -> Just i
      _ -> Nothing)
      headers

type ModuleMap = Map.HashMap T.Text H.ModuleName

type GenerateM = Reader ModuleMap

gInclude :: FilePath -> Include SourcePos -> IO (H.ImportDecl, ModuleMap)
gInclude dir i = do
  (Program headers _) <- loadFile (dir </> (T.unpack $ includePath i))
  let modName = H.ModuleName $ fromMaybe "" (extractNamespace headers) <> extractModuleName (T.unpack $ includePath i)
  let thriftModName = T.pack $ dropExtension $ T.unpack $ includePath i
  pure (H.ImportDecl modName True, Map.singleton thriftModName modName)

gDefinition :: Definition SourcePos -> GenerateM [H.Decl]
gDefinition def = case def of
  ConstDefinition _ -> pure []
  TypeDefinition ty -> gType ty
  ServiceDefinition _ -> pure []

gType :: Type SourcePos -> GenerateM [H.Decl]
gType ty = case ty of
  TypedefType t -> gTypedef t
  EnumType e -> gEnum e
  StructType s -> gStruct s
  _ -> pure []

gTypedef :: Typedef SourcePos -> GenerateM [H.Decl]
gTypedef def = do
  tyRef <- gTypeReference $ typedefTargetType def
  pure [H.TypeDecl (H.TypeName $ typedefName def) tyRef]

gTypeReference :: TypeReference SourcePos -> GenerateM H.Type
gTypeReference ref = case ref of
  StringType _ _ -> tyCon "Text"
  BinaryType _ _ -> tyCon "ByteString"
  BoolType _ _ -> tyCon "Bool"
  DoubleType _ _ -> tyCon "Double"
  I64Type _ _ -> tyCon "Int64"
  I32Type _ _ -> tyCon "Int32"
  ListType elemTy _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.Vector.Vector") <$> traverse gTypeReference [elemTy]
  MapType kTy vTy _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.HashMap.Strict.Map") <$> traverse gTypeReference [kTy, vTy]
  SetType ty _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.Set.Set") <$> traverse gTypeReference [ty]
  DefinedType ty _ -> case T.splitOn "." ty of
    xs@(x1:x2:_) -> do
      map <- ask
      case Map.lookup (mconcat $ init xs) map of
        Nothing -> tyCon ty
        Just (H.ModuleName n) -> pure $ H.TyCon $ H.TypeName $ n <> "." <> last xs
    _ -> tyCon ty
  ty -> error $ "Unsupported type: " <> show ty

  where tyCon = pure . H.TyCon . H.TypeName

gEnum :: A.Enum SourcePos -> GenerateM [H.Decl]
gEnum e = pure
  [ H.DataDecl (H.TypeName $ enumName e) (map gEnumDef $ enumValues e)
  ]

gEnumDef :: EnumDef SourcePos -> H.ConDecl
gEnumDef ed = H.ConDecl (H.Name $ enumDefName ed) []

gStruct :: Struct SourcePos -> GenerateM [H.Decl]
gStruct s = case structKind s of
  UnionKind -> undefined
  StructKind -> struct
  ExceptionKind -> struct

  where
    struct = do
      fields <- traverse gField $ structFields s
      pure $
        [H.DataDecl (H.TypeName $ structName s)
          [ H.RecConDecl (H.Name $ structName s) fields
          ]
        ]

gField :: Field SourcePos -> GenerateM (H.Name, H.Type)
gField f = do
  ty <- gTypeReference (fieldValueType f)
  pure (H.Name (fieldName f), ty)
