{-# LANGUAGE LambdaCase #-}
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
import           Prettyprinter
import           Prettyprinter.Render.Text
import           Data.Void
import           Language.Thrift.AST                   as A hiding ( exceptions
                                                                   , fields
                                                                   , headers
                                                                   , name
                                                                   , path
                                                                   , value
                                                                   )
import           Language.Thrift.Parser
import qualified Pinch.Generate.Pretty                 as H
import           Prelude                               hiding (mod)
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
  , sModulePrefix                 :: T.Text
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

getModuleName :: Settings -> [Header SourcePos] -> FilePath -> T.Text
getModuleName settings headers path =
  T.concat
    [ sModulePrefix settings
    , extractNamespace headers
    , extractName path
    ]
  where
    extractNamespace = fromMaybe "" . listToMaybe . mapMaybe getNamespaceHeader
    getNamespaceHeader = \case
      HeaderNamespace (Namespace l n _)
        | l == "hs" || l == "*"
        -> Just (n <> ".")
      _ -> Nothing

    extractName = T.concat . map capitalize . T.splitOn "_" . T.pack . takeBaseName

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
  (imports, tyMaps) <- unzip <$> traverse (gInclude s baseDir) incHeaders

  let tyMap = Map.unions tyMaps
  let (typeDecls, clientDecls, serverDecls) = unzip3 $ runReader (traverse gDefinition defs) $ Context tyMap s
  let mkMod suffix = H.Module (H.ModuleName $ modBaseName <> suffix)
        [ H.PragmaLanguage "TypeFamilies, DeriveGeneric, TypeApplications, OverloadedStrings"
        , H.PragmaOptsGhc "-w" ]
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
    modBaseName = getModuleName s headers inp
    baseDir = dropFileName inp
    incHeaders = mapMaybe (\x -> case x of
      HeaderInclude i -> Just i
      _ -> Nothing)
      headers
    impTypes = H.ImportDecl (H.ModuleName $ modBaseName <> ".Types") False H.IEverything
    defaultImports =
      [ H.ImportDecl (H.ModuleName "Prelude") True H.IEverything
      , H.ImportDecl (H.ModuleName "Control.Applicative") True H.IEverything
      , H.ImportDecl (H.ModuleName "Control.DeepSeq") True H.IEverything
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

gInclude :: Settings -> FilePath -> Include SourcePos -> IO (H.ImportDecl, ModuleMap)
gInclude s dir i = do
  -- TODO handle recursive includes ...
  (Program headers _) <- loadFile (dir </> (T.unpack $ includePath i))
  let modName = H.ModuleName $ getModuleName s headers (T.unpack $ includePath i) <> ".Types"
  let thriftModName = T.pack $ dropExtension $ T.unpack $ includePath i
  pure (H.ImportDecl modName True H.IEverything, Map.singleton thriftModName modName)

gDefinition :: Definition SourcePos -> GenerateM ([H.Decl], [H.Decl], [H.Decl])
gDefinition def = case def of
  ConstDefinition c -> (\x -> (x, [], [])) <$> gConst c
  TypeDefinition ty -> (\x -> (x, [], [])) <$> gType ty
  ServiceDefinition s -> gService s

gConst :: A.Const SourcePos -> GenerateM [H.Decl]
gConst constPos = do
  tyRef <- gTypeReference (constValueType constPos)
  value <- gConstValue (constValue constPos)
  pure
    [ H.TypeSigDecl name tyRef
    , H.FunBind [H.Match name [] value]
    ]
  where
    name = decapitalize (constName constPos)

gConstValue :: A.ConstValue SourcePos -> GenerateM H.Exp
gConstValue val = case val of
  ConstInt n _ -> pure (H.ELit (H.LInt n))
  ConstFloat n _ -> pure (H.ELit (H.LFloat n))
  ConstLiteral s _ -> pure (H.ELit (H.LString s))
  ConstIdentifier ident _
    | xs @(_:_:_) <- T.splitOn "." ident -> do
      moduleMap <- asks cModuleMap
      case Map.lookup (mconcat $ init xs) moduleMap of
        Nothing ->
          -- TODO this should probably be an error
          pure (H.EVar (decapitalize ident))
        Just (H.ModuleName n) ->
          pure $ H.EVar (n <> "." <> decapitalize (last xs))
    | otherwise -> pure $ H.EVar (decapitalize ident)
  ConstList xs _ -> do
    elems <- traverse gConstValue xs
    pure (H.EApp "Data.Vector.fromList" [H.EList elems])
  ConstMap xs _ -> do
    tuples <- traverse (\(k, v) -> H.ETuple <$> traverse gConstValue [k, v]) xs
    pure (H.EApp "Data.HashMap.Strict.fromList" [H.EList tuples])

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
  SListType _ _ -> tyCon "Data.Text.Text" -- http://thrift.apache.org/docs/idl#senum
  BinaryType _ _ -> tyCon "Data.ByteString.ByteString"
  BoolType _ _ -> tyCon "Prelude.Bool"
  DoubleType _ _ -> tyCon "Prelude.Double"
  ByteType _ _ -> tyCon "Data.Int.Int8"
  I16Type _ _ -> tyCon "Data.Int.Int16"
  I32Type _ _ -> tyCon "Data.Int.Int32"
  I64Type _ _ -> tyCon "Data.Int.Int64"
  ListType elemTy _ _ -> H.TyApp (H.TyCon $ "Data.Vector.Vector") <$> traverse gTypeReference [elemTy]
  MapType kTy vTy _ _ -> H.TyApp (H.TyCon $ "Data.HashMap.Strict.HashMap") <$> traverse gTypeReference [kTy, vTy]
  SetType ty _ _ -> H.TyApp (H.TyCon $ "Data.HashSet.HashSet") <$> traverse gTypeReference [ty]
  DefinedType ty _ -> case T.splitOn "." ty of
    xs@(_:_:_) -> do
      moduleMap <- asks cModuleMap
      case Map.lookup (mconcat $ init xs) moduleMap of
        Nothing -> tyCon $ capitalize ty
        Just (H.ModuleName n) -> pure $ H.TyCon $ n <> "." <> capitalize (last xs)
    _ -> tyCon $ capitalize ty

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
    , H.InstDecl (H.InstHead [] clNFData (H.TyCon tyName)) []
    ] ++ if sGenerateArbitrary settings then [
      H.InstDecl (H.InstHead [] clArbitrary (H.TyCon tyName)) [
        H.FunBind [ arbitrary ]
      ]
    ] else [])
  where
    tyName = enumName e
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
  UnionKind -> (++ [hashable, nfdata]) <$> unionDatatype tyName (structFields s) SRCNone
  StructKind -> (++ [hashable, nfdata]) <$> structDatatype tyName (structFields s)
  ExceptionKind -> (++ [hashable, nfdata, ex]) <$>  structDatatype tyName (structFields s)
  where
    tyName = structName s
    hashable = H.InstDecl (H.InstHead [] clHashable (H.TyCon tyName)) []
    nfdata = H.InstDecl (H.InstHead [] clNFData (H.TyCon tyName)) []
    ex = H.InstDecl (H.InstHead [] clException (H.TyCon tyName)) []


structDatatype :: T.Text -> [Field SourcePos] -> GenerateM [H.Decl]
structDatatype nm fs = do
  fields <- traverse (gField $ decapitalize $ nm) $ zip [1..] fs
  let (_, nms, tys, _) = unzip4 fields
  let stag = H.TypeDecl (H.TyApp tag [ H.TyCon nm ]) (H.TyCon $ "Pinch.TStruct")
  let pinch = H.FunBind
        [ H.Match "pinch" [H.PCon nm $ map H.PVar nms]
            ( H.EApp "Pinch.struct" [ H.EList $ flip map fields $ \(fId, fNm, _, fReq) ->
              let
                 op = if fReq then "Pinch..=" else "Pinch.?="
               in H.EInfix op (H.ELit $ H.LInt fId) (H.EVar fNm)
            ])
        ]
  let unpinch = H.FunBind
        [ H.Match "unpinch" [H.PVar "value"] $
            foldl'
              (\acc (fId, _, _, fReq) ->
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
              ( H.EApp "Pinch.pinch" [ "Pinch.Internal.RPC.Unit" ]
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
                      "(Pinch.unpinch v :: Pinch.Parser Pinch.Internal.RPC.Unit)"
              )
              fields
        ]
  let cons = map (\(_, nm', ty, _) -> H.ConDecl nm' [ ty ]) fields ++ case defCon of
        SRCNone -> []
        SRCVoid c -> [H.ConDecl (nm <> c) []]
  let arbitrary = H.FunBind
        [ H.Match "arbitrary" [] $
            H.EApp "Test.QuickCheck.oneof"
            [ H.EList $
              map
                (\(_, nm', _, _) ->
                  H.EInfix "Prelude.<$>" (H.EVar nm') "Test.QuickCheck.arbitrary"
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
  (nms, tys, handlers, calls, tyDecls) <- unzip5 <$> traverse gFunction (serviceFunctions s)
  let serverDecls =
        [ H.DataDecl serviceTyName [ H.RecConDecl serviceConName $ zip nms tys ] []
        , H.TypeSigDecl (prefix <> "_mkServer") (H.TyLam [H.TyCon serviceConName] (H.TyCon "Pinch.Server.ThriftServer"))
        , H.FunBind
          [ H.Match (prefix <> "_mkServer") [H.PVar "server"]
            ( H.ELet "functions"
              (H.EApp "Data.HashMap.Strict.fromList" [ H.EList handlers ] )
              ( H.EApp "Pinch.Server.createServer"
                [ (H.ELam ["nm"]
                    (H.EApp "Data.HashMap.Strict.lookup"
                      [ "nm", "functions" ]
                    )
                  )
                ]
              )
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

gFunction :: Function SourcePos -> GenerateM (H.Name, H.Type, H.Exp, [H.Decl], [H.Decl])
gFunction f = do
  argTys <- traverse (fmap snd . gFieldType) (functionParameters f)
  retType <- maybe (pure tyUnit) gTypeReference (functionReturnType f)


  argDataTy <- structDatatype argDataTyNm (functionParameters f)
  let catchers = map
        (\e -> H.EApp "Control.Exception.Handler"
          [ H.EInfix "Prelude.." "Prelude.pure" (H.EVar $ dtNm <> "_" <> capitalize (fieldName e))
          ]
        ) exceptions
  let resultField = fmap (\ty -> Field (Just 0) (Just Optional) ty "success" Nothing  [] Nothing (Pos.initialPos "")) (functionReturnType f)
  (resultDecls, resultDataTy) <- case (functionReturnType f, exceptions) of
    (Nothing, []) -> pure ([], H.TyCon $ if functionOneWay f then "()" else "Pinch.Internal.RPC.Unit")
    _ -> do
      let thriftResultInst = H.InstDecl (H.InstHead [] "Pinch.Internal.RPC.ThriftResult" (H.TyCon dtNm))
            [ H.TypeDecl (H.TyApp (H.TyCon "ResultType") [ H.TyCon dtNm ]) retType
            , H.FunBind (
               map (\e -> H.Match "unwrap" [H.PCon (dtNm <> "_" <> capitalize (fieldName e)) [H.PVar "x"]] (H.EApp "Control.Exception.throwIO" ["x"])) exceptions
               ++ [ H.Match "unwrap" [H.PCon (dtNm <> "_Success") (const (H.PVar "x") <$> maybeToList (functionReturnType f))] (H.EApp "Prelude.pure" (maybeToList $ ("x" <$ functionReturnType f) <|> pure "()"))]
              )
            , H.FunBind [H.Match "wrap" ["m"] (
              ( H.EApp "Control.Exception.catches"
                [ H.EInfix
                    (if isNothing (functionReturnType f) then "Prelude.<$" else "Prelude.<$>")
                    (H.EVar $ dtNm <> "_Success")
                    "m"
                , H.EList catchers
                ]
              ) ) ]
            ]
      dt <- unionDatatype
        dtNm
        (maybeToList resultField ++ exceptions)
        (case functionReturnType f of
          Nothing -> SRCVoid "_Success"
          _ -> SRCNone
        )
      pure ((thriftResultInst : dt), H.TyCon dtNm)


  let srvFunTy = H.TyLam ([H.TyCon "Pinch.Server.Context"] ++ argTys) (H.TyApp tyIO [retType])
  let clientFunTy = H.TyLam argTys (H.TyApp (H.TyCon "Pinch.Client.ThriftCall") [resultDataTy])
  let callSig = H.TypeSigDecl nm $ clientFunTy
  let call = H.FunBind
        [ H.Match nm ( map (H.PVar . fieldName) $ functionParameters f)
          ( H.EApp (if functionOneWay f then "Pinch.Client.TOneway" else "Pinch.Client.TCall")
            [ H.ELit $ H.LString $ functionName f
            , H.EApp (H.EVar argDataTyNm) $ map (H.EVar . fieldName) (functionParameters f)
            ]
          )
        ]
  let handler = H.ETuple
        [ H.ELit $ H.LString $ functionName f
        , H.EApp (if functionOneWay f then "Pinch.Server.OnewayHandler" else "Pinch.Server.CallHandler")
          [ H.ELam [ "ctx", H.PCon argDataTyNm (map H.PVar argVars) ] (
              (if functionOneWay f then id else
                (\c -> H.EApp (H.ETyApp "Pinch.Internal.RPC.wrap" [ resultDataTy ]) [c])
              )
              (H.EApp (H.EVar nm) (["server", "ctx"] ++ map H.EVar argVars))
            )
          ]
        ]

  pure ( nm, srvFunTy, handler, [callSig, call], (argDataTy ++ resultDecls))
  where
    nm = decapitalize $ functionName f
    dtNm = capitalize (functionName f) <> "_Result"
    argVars = take (length $ functionParameters f) $ map T.singleton ['a'..]
    argDataTyNm = capitalize $ functionName f <> "_Args"
    exceptions = concat $ maybeToList $ functionExceptions f

tag, tyUnit, tyIO :: H.Type
tag = H.TyCon $ "Tag"
tyUnit = H.TyCon $ "()"
tyIO = H.TyCon $ "Prelude.IO"

clPinchable, clHashable, clException, clArbitrary, clNFData :: H.ClassName
clPinchable = "Pinch.Pinchable"
clHashable = "Data.Hashable.Hashable"
clException = "Control.Exception.Exception"
clArbitrary = "Test.QuickCheck.Arbitrary"
clNFData = "Control.DeepSeq.NFData"

decapitalize :: T.Text -> T.Text
decapitalize s = if T.null s then "" else T.singleton (toLower $ T.head s) <> T.tail s

capitalize :: T.Text -> T.Text
capitalize s  = if T.null s then "" else T.singleton (toUpper $ T.head s) <> T.tail s

derivingShow, derivingEq, derivingOrd, derivingGenerics, derivingBounded :: H.Deriving
derivingShow = H.DeriveClass $ H.TyCon $ "Prelude.Show"
derivingEq = H.DeriveClass $ H.TyCon $ "Prelude.Eq"
derivingOrd = H.DeriveClass $ H.TyCon $ "Prelude.Ord"
derivingGenerics = H.DeriveClass $ H.TyCon $ "GHC.Generics.Generic"
derivingBounded = H.DeriveClass $ H.TyCon $ "Prelude.Bounded"
