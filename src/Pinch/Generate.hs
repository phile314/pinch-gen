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
import System.Directory
import System.IO

generate :: FilePath -> FilePath -> IO ()
generate inp out = do
  thrift <- loadFile inp

  mod <- gProgram inp thrift

  putDoc $ pretty mod

  let targetFile = out </> moduleFile mod

  createDirectoryIfMissing True (dropFileName targetFile)

  withFile targetFile WriteMode (\h -> hPutDoc h $ pretty mod)

moduleFile :: H.Module -> FilePath
moduleFile m =
  (foldr (</>) "" parts) <.> "hs"
  where
    (H.ModuleName n) = H.modName m
    parts = map T.unpack $ T.splitOn "." n

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
      throwIO $ err
    Right s -> pure s



gProgram :: FilePath -> Program SourcePos -> IO H.Module
gProgram inp (Program headers defs) = do
  (imports, maps) <- unzip <$> traverse (gInclude baseDir) incHeaders

  let map = Map.unions maps
  pure $
    H.Module (H.ModuleName $ ns <> extractModuleName inp)
    [H.PragmaLanguage "TypeFamilies"]
    (imports ++ defaultImports)
    (concat $ runReader (traverse gDefinition defs) map)

  where
    ns = fromMaybe "" $ extractNamespace headers
    baseDir = dropFileName inp
    incHeaders = mapMaybe (\x -> case x of
      HeaderInclude i -> Just i
      _ -> Nothing)
      headers
    defaultImports =
      [ H.ImportDecl (H.ModuleName "Prelude") True
      , H.ImportDecl (H.ModuleName "Pinch") True
      , H.ImportDecl (H.ModuleName "Data.Text") True
      , H.ImportDecl (H.ModuleName "Data.ByteString") True
      , H.ImportDecl (H.ModuleName "Data.Int") True
      , H.ImportDecl (H.ModuleName "Data.Vector") True
      , H.ImportDecl (H.ModuleName "Data.HashMap.Strict") True
      , H.ImportDecl (H.ModuleName "Data.HashSet") True
      ]

type ModuleMap = Map.HashMap T.Text H.ModuleName

type GenerateM = Reader ModuleMap

gInclude :: FilePath -> Include SourcePos -> IO (H.ImportDecl, ModuleMap)
gInclude dir i = do
  -- TODO handle recursive includes ...
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
  pure [H.TypeDecl (H.TyCon $ H.TypeName $ typedefName def) tyRef]

gTypeReference :: TypeReference SourcePos -> GenerateM H.Type
gTypeReference ref = case ref of
  StringType _ _ -> tyCon "Data.Text.Text"
  BinaryType _ _ -> tyCon "Data.ByteString.ByteString"
  BoolType _ _ -> tyCon "Prelude.Bool"
  DoubleType _ _ -> tyCon "Prelude.Double"
  I32Type _ _ -> tyCon "Data.Int.Int32"
  I64Type _ _ -> tyCon "Data.Int.Int64"
  ListType elemTy _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.Vector.Vector") <$> traverse gTypeReference [elemTy]
  MapType kTy vTy _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.HashMap.Strict.HashMap") <$> traverse gTypeReference [kTy, vTy]
  SetType ty _ _ -> H.TyApp (H.TyCon $ H.TypeName "Data.HashSet.HashSet") <$> traverse gTypeReference [ty]
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
  [ H.DataDecl tyName cons
  , H.InstDecl (H.InstHead [] clPinchable (H.TyCon tyName))
    [ H.TypeDecl (H.TyApp tag [ H.TyCon tyName ]) (H.TyCon $ H.TypeName "Pinch.TEnum")
    , H.FunBind pinch'
    , H.FunBind [unpinch']
    ]
  , H.InstDecl (H.InstHead [] (H.ClassName "Prelude.Enum") (H.TyCon tyName))
    [ H.FunBind fromEnum'
    , H.FunBind toEnum'
    ]
  ]
  where
    tyName = H.TypeName $ enumName e
    unpinch = H.Match (H.Name "unpinch") [H.PVar $ H.Name "x"]
      (H.EApp "Prelude.fmap" [ "Prelude.toEnum Prelude.. Prelude.fromIntegral", H.EApp "Pinch.unpinch" [ "x" ]])
    (cons, fromEnum', toEnum', pinch', unpinchAlts') = unzip5 $ map gEnumDef $ zip [0..] $ enumValues e

    defAlt = H.Alt (H.PVar $ H.Name "_")
      (H.EApp "Prelude.fail"
        [ H.EInfix (H.Name "Prelude.<>")
          (H.ELit $ H.LString $ "Unknown value for type " <> enumName e <> ": ")
          (H.EApp "Prelude.show" [ "val"] )
        ]
      )
    unpinch' = H.Match (H.Name "unpinch") [H.PVar $ H.Name "v"]
      ( H.EDo
        [ H.StmBind (Just $ H.PVar $ H.Name "val") (H.EApp "Pinch.unpinch" ["v"])
        , H.StmBind Nothing (H.ECase (H.ETyAnn "val" (H.TyCon $ H.TypeName "Data.Int.Int32")) (unpinchAlts' ++ [defAlt]) )
        ]
      )

gEnumDef :: (Integer, EnumDef SourcePos) -> (H.ConDecl, H.Match, H.Match, H.Match, H.Alt)
gEnumDef (i, ed) =
  ( H.ConDecl conName []
  , H.Match (H.Name "fromEnum") [H.PCon conName []] (H.ELit $ H.LInt index)
  , H.Match (H.Name "toEnum") [H.PLit $ H.LInt index] (H.EVar conName)
  , H.Match (H.Name "pinch") [H.PCon conName []]
    ( H.EApp "Pinch.pinch" 
      [ H.ETyAnn (H.ELit $ H.LInt index) (H.TyCon $ H.TypeName "Data.Int.Int32") ]
    )
  , H.Alt (H.PLit $ H.LInt index) (H.EApp "Prelude.pure" [ H.EVar conName ])
  )
  where
    index = fromMaybe i $ enumDefValue ed
    conName = H.Name $ enumDefName ed

gStruct :: Struct SourcePos -> GenerateM [H.Decl]
gStruct s = case structKind s of
  UnionKind -> undefined
  StructKind -> struct
  ExceptionKind -> struct

  where
    struct = do
      fields <- traverse (gField $ decapitalize $ structName s) $ structFields s
      pure $
        [H.DataDecl (H.TypeName $ structName s)
          [ H.RecConDecl (H.Name $ structName s) fields
          ]
        ]

gField :: T.Text -> Field SourcePos -> GenerateM (H.Name, H.Type)
gField prefix f = do
  ty <- gTypeReference (fieldValueType f)
  pure (H.Name (prefix <> "_" <> fieldName f), ty)

tag = H.TyCon $ H.TypeName "Tag"
clPinchable = H.ClassName "Pinch.Pinchable"

decapitalize s = T.singleton (toLower $ T.head s) <> T.tail s
