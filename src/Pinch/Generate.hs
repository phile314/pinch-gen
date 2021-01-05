{-# LANGUAGE OverloadedStrings #-}

module Pinch.Generate where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Reader
import qualified Data.ByteString                       as BS
import           Data.Char
import qualified Data.HashMap.Strict                   as Map
import           Data.List
import           Data.Maybe
import qualified Data.Text                             as T
import           Data.Text.Encoding
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Void
import           Language.Thrift.AST                   as A
import           Language.Thrift.Parser
import qualified Pinch.Generate.Pretty                 as H
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Megaparsec                       (SourcePos)
import qualified Text.Megaparsec                       as P
import qualified Text.Megaparsec.Error                 as E
import qualified Text.Megaparsec.Pos                   as Pos

data Settings
  = Settings
  { sHashableVectorInstanceModule :: T.Text
  , sGenerateArbitrary            :: Bool
  , sExtraImports                 :: [T.Text]
  } deriving (Show)

generate :: Settings -> FilePath -> FilePath -> IO ()
generate s inp out = do
  thrift <- loadFile inp

  mods <- gProgram s inp thrift



  forM_ mods $ \mod -> do
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
extractModuleName f = mconcat $ map capitalize parts
  where
    fileName = dropExtension $ takeFileName f
    parts = T.splitOn "_" $ T.pack fileName

extractNamespace :: [Header SourcePos] -> Maybe T.Text
extractNamespace headers =
  listToMaybe $ mapMaybe (\x -> case x of
    HeaderNamespace (Namespace l n _) | l == "hs" || l == "*" -> Just (n <> ".")
    _ -> Nothing
  ) headers


loadFile :: FilePath -> IO (Program SourcePos)
loadFile inp = do
  thrift <- parseFromFile' inp
  case thrift of
    Left err -> do
      putStrLn "Could not parse thrift file."
      throwIO $ err
    Right s -> pure s

parseFromFile' :: FilePath -> IO (Either (E.ParseErrorBundle T.Text Void) (Program SourcePos))
parseFromFile' path = P.runParser thriftIDL path . decodeUtf8 <$> BS.readFile path



gProgram :: Settings -> FilePath -> Program SourcePos -> IO [H.Module]
gProgram s inp (Program headers defs) = do
  (imports, tyMaps) <- unzip <$> traverse (gInclude baseDir) incHeaders

  let tyMap = Map.unions tyMaps
  let (typeDecls, clientDecls, serverDecls) = unzip3 $ runReader (traverse gDefinition defs) $ Context tyMap s
  let mkMod suffix = H.Module (H.ModuleName $ modBaseName <> suffix)
        [ H.PragmaLanguage "TypeFamilies, DeriveGeneric"
        , H.PragmaOptsGhc "-fno-warn-unused-imports -fno-warn-name-shadowing -fno-warn-unused-matches" ]
  pure $
    [ -- types
      mkMod ".Types"
      (imports ++ defaultImports ++ map
        (\n -> H.ImportDecl (H.ModuleName n) True H.IEverything)
        (sExtraImports s ++ if sGenerateArbitrary s then [ "Test.QuickCheck" ] else [])
      )
      (concat typeDecls)
    , -- client
      mkMod ".Client"
      ( [ impTypes
        , H.ImportDecl (H.ModuleName "Pinch.Client") True H.IEverything
        ] ++ imports ++ defaultImports)
      (concat clientDecls)
    , -- server
      mkMod ".Server"
      ( [ impTypes
        , H.ImportDecl (H.ModuleName "Pinch.Server") True H.IEverything
        ] ++ imports ++ defaultImports)
      (concat serverDecls)
    ]

  where
    ns = fromMaybe "" $ extractNamespace headers
    modBaseName = ns <> extractModuleName inp
    baseDir = dropFileName inp
    incHeaders = mapMaybe (\x -> case x of
      HeaderInclude i -> Just i
      _ -> Nothing)
      headers
    impTypes = H.ImportDecl (H.ModuleName $ modBaseName <> ".Types") False H.IEverything
    defaultImports =
      [ H.ImportDecl (H.ModuleName "Prelude") True H.IEverything
      , H.ImportDecl (H.ModuleName "Control.Applicative") True H.IEverything
      , H.ImportDecl (H.ModuleName "Control.Exception") True H.IEverything
      , H.ImportDecl (H.ModuleName "Pinch") True H.IEverything
      , H.ImportDecl (H.ModuleName "Pinch.Server") True H.IEverything
      , H.ImportDecl (H.ModuleName "Pinch.Internal.RPC") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.Text") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.ByteString") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.Int") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.Vector") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.HashMap.Strict") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.HashSet") True H.IEverything
      , H.ImportDecl (H.ModuleName "GHC.Generics") True H.IEverything
      , H.ImportDecl (H.ModuleName "Data.Hashable") True H.IEverything
      , H.ImportDecl (H.ModuleName $ sHashableVectorInstanceModule s) False (H.IJust [])
      ]

type ModuleMap = Map.HashMap T.Text H.ModuleName

data Context
  = Context
  { cModuleMap :: ModuleMap
  , cSettings  :: Settings
  }

type GenerateM = Reader Context

gInclude :: FilePath -> Include SourcePos -> IO (H.ImportDecl, ModuleMap)
gInclude dir i = do
  -- TODO handle recursive includes ...
  (Program headers _) <- loadFile (dir </> (T.unpack $ includePath i))
  let modName = H.ModuleName $ fromMaybe "" (extractNamespace headers) <> extractModuleName (T.unpack $ includePath i) <> ".Types"
  let thriftModName = T.pack $ dropExtension $ T.unpack $ includePath i
  pure (H.ImportDecl modName True H.IEverything, Map.singleton thriftModName modName)

gDefinition :: Definition SourcePos -> GenerateM ([H.Decl], [H.Decl], [H.Decl])
gDefinition def = case def of
  ConstDefinition _ -> pure ([], [], [])
  TypeDefinition ty -> (\x -> (x, [], [])) <$> gType ty
  ServiceDefinition s -> gService s

gType :: Type SourcePos -> GenerateM [H.Decl]
gType ty = case ty of
  TypedefType t -> gTypedef t
  EnumType e -> gEnum e
  StructType s -> gStruct s
  _ -> pure []

gTypedef :: Typedef SourcePos -> GenerateM [H.Decl]
gTypedef def = do
  tyRef <- gTypeReference $ typedefTargetType def
  pure [H.TypeDecl (H.TyCon $ capitalize $ typedefName $ def) tyRef]

gTypeReference :: TypeReference SourcePos -> GenerateM H.Type
gTypeReference ref = case ref of
  StringType _ _ -> tyCon "Data.Text.Text"
  BinaryType _ _ -> tyCon "Data.ByteString.ByteString"
  BoolType _ _ -> tyCon "Prelude.Bool"
  DoubleType _ _ -> tyCon "Prelude.Double"
  I16Type _ _ -> tyCon "Data.Int.Int16"
  I32Type _ _ -> tyCon "Data.Int.Int32"
  I64Type _ _ -> tyCon "Data.Int.Int64"
  ListType elemTy _ _ -> H.TyApp (H.TyCon $ "Data.Vector.Vector") <$> traverse gTypeReference [elemTy]
  MapType kTy vTy _ _ -> H.TyApp (H.TyCon $ "Data.HashMap.Strict.HashMap") <$> traverse gTypeReference [kTy, vTy]
  SetType ty _ _ -> H.TyApp (H.TyCon $ "Data.HashSet.HashSet") <$> traverse gTypeReference [ty]
  DefinedType ty _ -> case T.splitOn "." ty of
    xs@(x1:x2:_) -> do
      map <- asks cModuleMap
      case Map.lookup (mconcat $ init xs) map of
        Nothing -> tyCon $ capitalize ty
        Just (H.ModuleName n) -> pure $ H.TyCon $ n <> "." <> capitalize (last xs)
    _ -> tyCon $ capitalize ty
  ty -> error $ "Unsupported type: " <> show ty

  where tyCon = pure . H.TyCon

gEnum :: A.Enum SourcePos -> GenerateM [H.Decl]
gEnum e = do
  settings <- asks cSettings
  pure (
    [ H.DataDecl tyName cons [ derivingEq, derivingOrd, derivingGenerics, derivingShow, derivingBounded ]
    , H.InstDecl (H.InstHead [] clPinchable (H.TyCon tyName))
      [ H.TypeDecl (H.TyApp tag [ H.TyCon tyName ]) (H.TyCon $ "Pinch.TEnum")
      , H.FunBind pinch'
      , H.FunBind [unpinch']
      ]
    , H.InstDecl (H.InstHead [] "Prelude.Enum" (H.TyCon tyName))
      [ H.FunBind fromEnum'
      , H.FunBind (toEnum' ++ [toEnumDef])
      ]
    , H.InstDecl (H.InstHead [] clHashable (H.TyCon tyName)) []
    ] ++ if sGenerateArbitrary settings then [
      H.InstDecl (H.InstHead [] clArbitrary (H.TyCon tyName)) [
        H.FunBind [ arbitrary ]
      ]
    ] else [])
  where
    tyName = enumName e
    unpinch = H.Match "unpinch" [H.PVar "x"]
      (H.EApp "Prelude.fmap" [ "Prelude.toEnum Prelude.. Prelude.fromIntegral", H.EApp "Pinch.unpinch" [ "x" ]])
    (cons, fromEnum', toEnum', pinch', unpinchAlts') = unzip5 $ map gEnumDef $ zip [0..] $ enumValues e

    defAlt = H.Alt (H.PVar "_")
      (H.EApp "Prelude.fail"
        [ H.EInfix "Prelude.<>"
          (H.ELit $ H.LString $ "Unknown value for type " <> enumName e <> ": ")
          (H.EApp "Prelude.show" [ "val"] )
        ]
      )
    toEnumDef = H.Match "toEnum" [H.PVar "_"] (H.EApp "Prelude.error" [ H.ELit $ H.LString $ "Unknown value for enum " <> enumName e <> "." ])
    unpinch' = H.Match "unpinch" [H.PVar "v"]
      ( H.EDo
        [ H.StmBind (Just $ H.PVar "val") (H.EApp "Pinch.unpinch" ["v"])
        , H.StmBind Nothing (H.ECase (H.ETyAnn "val" (H.TyCon $ "Data.Int.Int32")) (unpinchAlts' ++ [defAlt]) )
        ]
      )
    arbitrary = H.Match "arbitrary" [] (
        H.EApp "Test.QuickCheck.elements" [H.EList $ map (H.EVar . enumDefName) $ enumValues e]
      )

gEnumDef :: (Integer, EnumDef SourcePos) -> (H.ConDecl, H.Match, H.Match, H.Match, H.Alt)
gEnumDef (i, ed) =
  ( H.ConDecl conName []
  , H.Match "fromEnum" [H.PCon conName []] (H.ELit $ H.LInt index)
  , H.Match "toEnum" [H.PLit $ H.LInt index] (H.EVar conName)
  , H.Match "pinch" [H.PCon conName []]
    ( H.EApp "Pinch.pinch"
      [ H.ETyAnn (H.ELit $ H.LInt index) (H.TyCon $ "Data.Int.Int32") ]
    )
  , H.Alt (H.PLit $ H.LInt index) (H.EApp "Prelude.pure" [ H.EVar conName ])
  )
  where
    index = fromMaybe i $ enumDefValue ed
    conName = enumDefName ed

gStruct :: Struct SourcePos -> GenerateM [H.Decl]
gStruct s = case structKind s of
  UnionKind -> (++ [hashable]) <$> unionDatatype tyName (structFields s) SRCNone
  StructKind -> (++ [hashable]) <$> structDatatype tyName (structFields s)
  ExceptionKind -> (++ [hashable, ex]) <$>  structDatatype tyName (structFields s)
  where
    tyName = structName s
    hashable = H.InstDecl (H.InstHead [] clHashable (H.TyCon tyName)) []
    ex = H.InstDecl (H.InstHead [] clException (H.TyCon tyName)) []


structDatatype :: T.Text -> [Field SourcePos] -> GenerateM [H.Decl]
structDatatype nm fs = do
  fields <- traverse (gField $ decapitalize $ nm) $ zip [1..] fs
  let (_, nms, tys, _) = unzip4 fields
  let stag = H.TypeDecl (H.TyApp tag [ H.TyCon nm ]) (H.TyCon $ "Pinch.TStruct")
  let pinch = H.FunBind
        [ H.Match "pinch" [H.PCon nm $ map H.PVar nms]
            ( H.EApp "Pinch.struct" [ H.EList $ flip map fields $ \(fId, fNm, fTy, fReq) ->
              let
                 op = if fReq then "Pinch..=" else "Pinch.?="
               in H.EInfix op (H.ELit $ H.LInt fId) (H.EVar fNm)
            ])
        ]
  let unpinch = H.FunBind
        [ H.Match "unpinch" [H.PVar "value"] $
            foldl'
              (\acc (fId, fNm, fTy, fReq) ->
                H.EInfix "Prelude.<*>" acc (
                  H.EInfix (if fReq then "Pinch..:" else "Pinch..:?")
                    "value"
                    (H.ELit $  H.LInt fId)
                )
              )
              (H.EApp "Prelude.pure" [ H.EVar $ nm ] )
              fields
        ]
  let arbitrary = H.FunBind
        [ H.Match "arbitrary" [] $
            foldl'
              (\acc _ ->
                H.EInfix "Prelude.<*>" acc (
                  "Test.QuickCheck.arbitrary"
                )
              )
              (H.EApp "Prelude.pure" [ H.EVar $ nm ] )
              fields
        ]
  settings <- asks cSettings
  pure $
    [ H.DataDecl nm
      [ H.RecConDecl nm (zip nms tys)
      ]
      [ derivingEq, derivingGenerics, derivingShow ]
    , H.InstDecl (H.InstHead [] clPinchable (H.TyCon nm)) [ stag, pinch, unpinch ]
    ] ++ (if sGenerateArbitrary settings then [
      H.InstDecl (H.InstHead [] clArbitrary (H.TyCon nm)) [ arbitrary ]
    ] else [])

data ServiceResultCon = SRCNone | SRCVoid H.Name

unionDatatype :: T.Text -> [Field SourcePos] -> ServiceResultCon -> GenerateM [H.Decl]
unionDatatype nm fs defCon = do
  fields <- traverse (gField $ nm) $ zip [1..] $ map (\f -> f { fieldRequiredness = Just Required, fieldName = capitalize (fieldName f) } ) fs
  let stag = H.TypeDecl (H.TyApp tag [ H.TyCon nm ]) (H.TyCon $ "Pinch.TUnion")
  let pinch = H.FunBind $
        map (\(fId, fNm, _, _) ->
          H.Match "pinch" [H.PCon fNm [H.PVar "x"]]
            ( H.EApp "Pinch.union" [ H.ELit $ H.LInt  fId, "x"]
            )

        ) fields ++ case defCon of
          SRCNone -> []
          SRCVoid c ->
            [ H.Match "pinch" [H.PCon (nm <> c) []]
              ( H.EApp "Pinch.pinch" [ "Pinch.Unit" ]
              )
            ]
  let unpinch = H.FunBind
        [ H.Match "unpinch" [H.PVar "v"] $
            foldl'
              (\acc (fId, fNm, _, _) ->
                H.EInfix "Control.Applicative.<|>" acc (
                  H.EInfix "Prelude.<$>"
                    (H.EVar fNm)
                    (H.EInfix "Pinch..:" "v" $ H.ELit $ H.LInt fId)
                )
              )
              ( case defCon of
                  SRCNone -> "Control.Applicative.empty"
                  SRCVoid c ->
                    H.EInfix "Prelude.<$" (H.EVar (nm <> c))
                      "(Pinch.unpinch v :: Pinch.Parser Pinch.Unit)"
              )
              fields
        ]
  let cons = map (\(_, nm, ty, _) -> H.ConDecl nm [ ty ]) fields ++ case defCon of
        SRCNone -> []
        SRCVoid c -> [H.ConDecl (nm <> c) []]
  let arbitrary = H.FunBind
        [ H.Match "arbitrary" [] $
            H.EApp "Test.QuickCheck.oneof"
            [ H.EList $
              map
                (\(_, nm, _, _) ->
                  H.EInfix "Prelude.<$>" (H.EVar nm) "Test.QuickCheck.arbitrary"
                )
                fields
            ]
        ]
  settings <- asks cSettings
  pure $
    [ H.DataDecl nm
      cons
      [ derivingEq, derivingGenerics, derivingShow ]
      , H.InstDecl (H.InstHead [] clPinchable (H.TyCon nm)) [ stag, pinch, unpinch ]
    ] ++ (if sGenerateArbitrary settings then [
      H.InstDecl (H.InstHead [] clArbitrary (H.TyCon nm)) [ arbitrary ]
    ] else [])

gField :: T.Text -> (Integer, Field SourcePos) -> GenerateM (Integer, H.Name, H.Type, Bool)
gField prefix (i, f) = do
  (req, ty) <- gFieldType f
  let index = fromMaybe i (fieldIdentifier f)
  pure (index, prefix <> "_" <> fieldName f, ty, req)


gService :: Service SourcePos -> GenerateM ([H.Decl], [H.Decl], [H.Decl])
gService s = do
  (nms, tys, alts, calls, tyDecls) <- unzip5 <$> traverse gFunction (serviceFunctions s)
  let serverDecls =
        [ H.DataDecl serviceTyName [ H.RecConDecl serviceConName $ zip nms tys ] []
        , H.TypeSigDecl (prefix <> "_mkServer") (H.TyLam [H.TyCon serviceConName] (H.TyCon "Pinch.Server.ThriftServer"))
        , H.FunBind
          [ H.Match (prefix <> "_mkServer") [H.PVar "server"]
            ( H.EApp "Pinch.Server.ThriftServer"
              [ H.ELam ["ctx", "m"]
                (
                  H.ECase (H.EApp "Pinch.messageName" ["m"]) (alts ++ [
                    H.Alt (H.PVar "_") (H.EApp "Prelude.pure" [H.EApp "Pinch.Server.unknownMethodError" ["m"]])
                  ])
                )
              ]
            )
          ]
        ]
  pure (concat tyDecls, concat calls, serverDecls)
  where
    serviceTyName = capitalize $ serviceName s
    serviceConName = capitalize $ serviceName s
    prefix = decapitalize $ serviceName s

gFieldType :: Field SourcePos -> GenerateM (Bool, H.Type)
gFieldType f = do
  ty <- gTypeReference (fieldValueType f)
  case fieldRequiredness f of
    Just Optional -> pure (False, H.TyApp (H.TyCon $ "Prelude.Maybe") [ ty ])
    _ -> pure (True, ty)

gFunction :: Function SourcePos -> GenerateM (H.Name, H.Type, H.Alt, [H.Decl], [H.Decl])
gFunction f = do
  argTys <- traverse (fmap snd . gFieldType) (functionParameters f)
  retType <- maybe (pure tyUnit) gTypeReference (functionReturnType f)


  argDataTy <- structDatatype argDataTyNm (functionParameters f)
  let resultField = fmap (\ty -> Field (Just 0) (Just Optional) ty "success" Nothing  [] Nothing (Pos.initialPos "")) (functionReturnType f)
  (resultDecls, resultDataTy) <- case (functionReturnType f, exceptions) of
    (Nothing, []) -> pure ([], H.TyCon "Pinch.Unit")
    _ -> do
      let thriftResultInst = H.InstDecl (H.InstHead [] "Pinch.ThriftResult" (H.TyCon dtNm))
            [ H.TypeDecl (H.TyApp (H.TyCon "ResultType") [ H.TyCon dtNm ]) retType
            , H.FunBind (
               map (\e -> H.Match "toEither" [H.PCon (dtNm <> "_" <> capitalize (fieldName e)) [H.PVar "x"]] (H.EApp "Prelude.Left" [H.EApp "Control.Exception.SomeException" ["x"]])) exceptions
               ++ [ H.Match "toEither" [H.PCon (dtNm <> "_Success") (const (H.PVar "x") <$> maybeToList (functionReturnType f))] (H.EApp "Prelude.Right" (maybeToList $ ("x" <$ functionReturnType f) <|> pure "()"))]
              )
            , H.FunBind [H.Match "wrapException" [] (
                H.EApp "Pinch.Internal.RPC.wrapExceptions" [
                  H.EList (map (\e ->
                    H.EApp "Pinch.Internal.RPC.Wrapper" [(H.EVar $ dtNm <> "_" <> capitalize (fieldName e))]
                  ) exceptions)
                ]
                )
              ]
            ]
      dt <- unionDatatype
        dtNm
        (maybeToList resultField ++ exceptions)
        (case functionReturnType f of
          Nothing -> SRCVoid "_Success"
          _ -> SRCNone
        )
      pure ((thriftResultInst : dt), H.TyCon dtNm)


  let srvFunTy = H.TyLam [H.TyCon "Pinch.Server.Context", H.TyCon argDataTyNm] (H.TyApp tyIO [retType])
  let clientFunTy = H.TyLam argTys (H.TyApp (H.TyCon "Pinch.Client.ThriftCall") [resultDataTy])
  let callSig = H.TypeSigDecl nm $ clientFunTy
  let callArgs = map (\(i, p) ->
        let
          op = if fieldRequiredness p == Just Optional then "Pinch.?=" else "Pinch..="
          index = H.ELit $ H.LInt $ fromMaybe i (fieldIdentifier p)
        in H.EInfix op index (H.EVar $ fieldName p)
        ) (zip [1..] $ functionParameters f)
  let call = H.FunBind
        [ H.Match nm ( map (H.PVar . fieldName) $ functionParameters f)
          ( H.EApp "Pinch.Client.ThriftCall"
            [ H.EApp "Pinch.mkMessage" [ H.ELit $ H.LString $ functionName f, "Pinch.Call", H.ELit $ H.LInt 0, H.EApp "Pinch.struct" [ H.EList $ callArgs ] ]
            ]
          )
        ]

  pure ( nm, srvFunTy, alt, [callSig, call], (argDataTy ++ resultDecls))
  where
    nm = decapitalize $ functionName f
    dtNm = capitalize (functionName f) <> "_Result"
    alt = H.Alt (H.PLit $ H.LString $ functionName f)
      (H.EApp "Pinch.Server.runServiceMethod"
        [ H.EInfix "Prelude.<$>" (H.EVar $ dtNm <> "_Success") (H.EApp (H.EVar nm) [ "server", "ctx", "m" ] ) ]
      )
    argDataTyNm = capitalize $ functionName f <> "_Args"
    exceptions = concat $ maybeToList $ functionExceptions f

tag = H.TyCon $ "Tag"
clPinchable = "Pinch.Pinchable"
clHashable = "Data.Hashable.Hashable"
tyUnit = H.TyCon $ "()"
tyIO = H.TyCon $ "Prelude.IO"
clException = "Control.Exception.Exception"
clArbitrary = "Test.QuickCheck.Arbitrary"

decapitalize :: T.Text -> T.Text
decapitalize s = if T.null s then "" else T.singleton (toLower $ T.head s) <> T.tail s

capitalize :: T.Text -> T.Text
capitalize s  = if T.null s then "" else T.singleton (toUpper $ T.head s) <> T.tail s

derivingShow = H.DeriveClass $ H.TyCon $ "Prelude.Show"
derivingEq = H.DeriveClass $ H.TyCon $ "Prelude.Eq"
derivingOrd = H.DeriveClass $ H.TyCon $ "Prelude.Ord"
derivingGenerics = H.DeriveClass $ H.TyCon $ "GHC.Generics.Generic"
derivingBounded = H.DeriveClass $ H.TyCon $ "Prelude.Bounded"
