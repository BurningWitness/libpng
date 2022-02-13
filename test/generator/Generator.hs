{-# LANGUAGE DataKinds
           , FlexibleInstances
           , GADTs
           , KindSignatures
           , OverloadedStrings
           , StandaloneDeriving #-}

module Generator where

import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as BSL
import           Data.ByteString.Builder
import           Data.Char
import           Data.Foldable (asum)
import           Data.List
import           Data.Maybe
import           Data.Semigroup (mtimesDefault)
import           Parser



newtype Pattern = Pattern { unPattern :: ByteString }

toConstants :: [Define] -> [Pattern]
toConstants = foldMap $ \(Define typ name) -> case typ of
                                                Function -> []
                                                Constant -> [Pattern name]

longestPattern :: [Pattern] -> Int
longestPattern = maximum . fmap (BSC.length . unPattern)

putPatterns' :: Builder -> Builder -> [Pattern] -> Builder
putPatterns' _          _       []                    = mempty
putPatterns' constraint command [Pattern a]           =
  mconcat
    [ "pattern ", byteString a, " :: (", constraint, " a, Ord a) => a\n"
    , "pattern ", byteString a, " = ", command, " ", byteString a
    ]
putPatterns' constraint command as@(Pattern a : rest) =
  mconcat
    [ "pattern ", byteString a, char8 '\n', putType rest
    , foldMap putPattern as
    ]
  where
    putType (Pattern a : as) = "      , " <> byteString a <> char8 '\n' <> putType as
    putType              []  = "     :: (" <> constraint <> " a, Ord a) => a\n"

    putPattern (Pattern name) =
      mconcat
        [ "pattern "
        , byteString name
        , mtimesDefault (longestPattern as - BSC.length name) " "
        , " = #", command, " "
        , byteString name
        , char8 '\n'
        ]

putPatterns
  , putPatternsStr
 :: [Pattern] -> Builder
putPatterns    = putPatterns' "Num" "const"
putPatternsStr = putPatterns' "IsString" "const_str"



data Inner = Inner | Outer

data Datatype (i :: Inner) where
  Basic           :: ByteString -> Datatype i
  Custom          :: ByteString -> Maybe ([(ByteString, Datatype 'Outer)], ByteString) -> Datatype i
  FunctionPointer :: ByteString -> [Datatype 'Outer] -> Datatype 'Outer -> Datatype i
  Pointer         :: Datatype 'Inner -> Datatype 'Outer
  ConstPointer    :: Datatype 'Inner -> Datatype 'Outer
  RPointer        :: Datatype 'Inner -> Datatype 'Outer
  ConstRPointer   :: Datatype 'Inner -> Datatype 'Outer
  Pointer2        :: Datatype 'Inner -> Datatype 'Outer
  ConstPointer2   :: Datatype 'Inner -> Datatype 'Outer
  Pointer3        :: Datatype 'Inner -> Datatype 'Outer

deriving instance Show (Datatype i)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

camelCase :: ByteString -> ByteString
camelCase = BSL.toStrict . toLazyByteString . camelCase'

camelCase' :: ByteString -> Builder
camelCase' bs | Just (a, as) <- BSC.uncons bs = char8 (toUpper a) <> camel as
              | otherwise                     = mempty
  where
    camel cs | BSC.null cs = mempty
             | otherwise   = let (bef, aft) = BSC.span (/= '_') cs
                                 next | Just (_, as) <- BSC.uncons aft
                                      , Just (a, rest) <- BSC.uncons as = char8 (toUpper a) <> camel rest
                                      | otherwise           = mempty
                             in byteString bef <> next

camelCasePrefixed :: ByteString -> ByteString -> ByteString
camelCasePrefixed name bs
  | Just (a, as) <- BSC.uncons name = BSL.toStrict . toLazyByteString $
                                        char8 a <> shorten name <> camelCase' bs
  | otherwise                       = mempty
  where
    shorten cs | BSC.null cs = mempty
               | otherwise   = let (bef, aft) = BSC.break (\a -> a == '_' || isDigit a) cs
                                   aft' | Just (a, as) <- BSC.uncons aft
                                        , Just (b, rest) <- BSC.uncons as =
                                            if a == '_'
                                              then char8 b <> shorten rest
                                              else char8 a <> shorten as
                                        | otherwise           = mempty
                               in aft'



getBaseType :: Record (Maybe ByteString) -> Maybe (Datatype 'Outer)
getBaseType (Record name ptr typ isConst isStruct) = do
  base <- lookup typ
            [ (,) "void"         $ Basic "()"
            , (,) "size_t"       $ Basic "#{type size_t}"
            , (,) "int"          $ Basic "#{type int}"
            , (,) "unsigned int" $ Basic "#{type unsigned int}"
            , (,) "char"         $ Basic "#{type char}"
            , (,) "float"        $ Basic "#{type float}"
            , (,) "double"       $ Basic "#{type double}"
            , (,) "FILE"         $ Basic "CFile"
            , (,) "jmp_buf"      $ Basic "CJmpBuf"
            , (,) "tm"           $ Basic "CTm"
            , (,) "time_t"       $ Basic "#{type time_t}"
            ]
  levelUp ptr isConst 0 base



toOuter :: Datatype 'Inner -> Datatype 'Outer
toOuter (Basic a) = Basic a
toOuter (Custom a b) = Custom a b
toOuter (FunctionPointer a b c) = FunctionPointer a b c

toInner :: Datatype 'Outer -> Datatype 'Inner
toInner (Basic a) = Basic a
toInner (Custom a b) = Custom a b
toInner (FunctionPointer a b c) = FunctionPointer a b c
toInner (Pointer a) = a
toInner (RPointer a) = a
toInner (ConstPointer a) = a
toInner (ConstRPointer a) = a
toInner (Pointer2 a) = a
toInner (ConstPointer2 a) = a
toInner (Pointer3 a) = a

isBasic :: Datatype a -> Bool
isBasic (Basic a) = True
isBasic _         = False



levelUp :: Maybe (Int, Bool) -> Bool -> Int -> Datatype 'Inner -> Maybe (Datatype 'Outer)
levelUp Nothing           _     0 = Just . toOuter
levelUp (Just (1, r))     c     0 = Just . case (c, r) of
                                             (True , True ) -> ConstRPointer
                                             (True , False) -> ConstPointer
                                             (False, True ) -> RPointer
                                             (False, False) -> Pointer
levelUp (Just (2, False)) c     0 | c         = Just . ConstPointer2
                                  | otherwise = Just . Pointer2
levelUp Nothing           c     1 = levelUp (Just (1, False)) c 0
levelUp (Just (1, False)) c     1 = levelUp (Just (2, False)) c 0
levelUp Nothing           c     2 = levelUp (Just (1, False)) c 1
levelUp (Just (1, False)) False 2 = Just . Pointer3
levelUp Nothing           False 3 = Just . Pointer3
levelUp _                 _     _ = const Nothing

getPngType :: Record a -> Maybe (Datatype 'Outer)
getPngType (Record _ ptr typ isConst _) = do
  (isReallyConst, lvl, base) <-
    asum
      [ (isConst, 0, Basic ("#{type " <> typ <> "}")) <$ do
          guard $ elem typ [ "png_byte"
                           , "png_int_16"
                           , "png_uint_16"
                           , "png_int_32"
                           , "png_uint_32"
                           , "png_size_t"
                           , "png_ptrdiff_t"
                           , "png_size_t"
                           , "png_ptrdiff_t"
                           , "png_alloc_size_t"
                           , "png_fixed_point"
                           ]
      , (False, 1, Basic "()") <$ do guard $ typ == "png_voidp"
      , (False, 1, Basic "CFile") <$ do guard $ typ == "png_FILE_p"
      , (\val -> (False, 1, Basic $ "#{type " <> val <> "}")) <$> do
          lookup typ $ swap <$> [ (,) "png_byte"        "png_bytep"
                                , (,) "png_uint_32"     "png_uint_32p"
                                , (,) "png_int_32"      "png_int_32p"
                                , (,) "png_uint_16"     "png_uint_16p"
                                , (,) "png_int_16"      "png_int_16p"
                                , (,) "char"            "png_charp"
                                , (,) "png_fixed_point" "png_fixed_point_p"
                                , (,) "size_t"          "png_size_tp"
                                , (,) "double"          "png_doublep"
                                ]
      , (True, 1, Basic "()") <$ do guard $ typ == "png_const_voidp"
      , (\val -> (True, 1, Basic $ "#{type " <> val <> "}")) <$> do
          lookup typ $ swap <$> [ (,) "png_byte"        "png_const_bytep"
                                , (,) "png_uint_32"     "png_const_uint_32p"
                                , (,) "png_int_32"      "png_const_int_32p"
                                , (,) "png_uint_16"     "png_const_uint_16p"
                                , (,) "png_int_16"      "png_const_int_16p"
                                , (,) "char"            "png_const_charp"
                                , (,) "png_fixed_point" "png_const_fixed_point_p"
                                , (,) "size_t"          "png_const_size_tp"
                                , (,) "double"          "png_const_doublep"
                                ]
      , (\val -> (False, 2, Basic $ "#{type " <> val <> "}")) <$> do
          lookup typ $ swap <$> [ (,) "png_byte"        "png_bytepp"
                                , (,) "png_uint_32"     "png_uint_32pp"
                                , (,) "png_int_32"      "png_int_32pp"
                                , (,) "png_uint_16"     "png_uint_16pp"
                                , (,) "png_int_16"      "png_int_16pp"
                                , (,) "char"            "png_charpp"
                                , (,) "png_fixed_point" "png_fixed_point_pp"
                                , (,) "double"          "png_doublepp"
                                ]
      , (True , 2, Basic "#{type char}") <$ do guard $ typ == "png_const_charpp"
      , (False, 3, Basic "#{type char}") <$ do guard $ typ == "png_charppp"
      ]
  levelUp ptr isReallyConst lvl base


mkCallbackType :: Pile -> Callback ByteString -> Maybe (Datatype i)
mkCallbackType pile (Callback name ret args) =
  FunctionPointer name
    <$> sequence (getType pile <$> args)
    <*> getType pile (Record Nothing Nothing ret False False)

callbackTypes :: Pile -> Maybe [Datatype 'Outer]
callbackTypes pile =
  sequence $ mkCallbackType pile <$> pCallbacks pile

getCallbackType :: Pile -> ByteString -> Maybe (Datatype 'Outer)
getCallbackType pile bs = do
  found <- find ((bs ==) . cName) $ pCallbacks pile
  mkCallbackType pile found



getExtraType :: Pile -> ByteString -> Maybe (Datatype 'Outer)
getExtraType pile bs = do
  Record _ ns typ isConst _ <- find ((bs ==) . rName) . filter filterTypedef $ pTypedefs pile
  base <- getCustomType' pile typ
  levelUp ns isConst 0 base



mkCustomType' :: Pile -> Struct ByteString -> Record (Maybe ByteString) -> Maybe (Datatype a)
mkCustomType' pile (Struct name rs as) (Record _ _ typ _ _) = do
  args <- sequence $ flip fmap rs $ \r ->
                            (,) (rName r) <$> getType pile (Just <$> r)
  ret <- Just . rType <$> headMay as
  Just . Custom (fromMaybe (typ <> "_struct") name) $ case args of
                                                        [] -> Nothing
                                                        _  -> (,) args <$> ret

mkCustomType :: Pile -> Struct ByteString -> Record (Maybe ByteString) -> Maybe (Datatype 'Outer)
mkCustomType pile s@(Struct name rs (r:_)) (Record _ ptr _ isConst _) = do
  base <- mkCustomType' pile s r
  levelUp ptr isConst 0 base



headMay :: [a] -> Maybe a
headMay as | a:_ <- as = Just a
           | otherwise = Nothing

customTypes :: Pile -> Maybe [Datatype 'Outer]
customTypes pile =
  sequence . flip fmap (pStructs pile) $ \s -> do
                    assign <- headMay $ sAssign s
                    res <- mkCustomType pile s assign
                    Just . toOuter $ toInner res

getCustomType' :: Pile -> ByteString -> Maybe (Datatype a)
getCustomType' pile bs = do
  (ftype, found) <- find ((==) bs . rType . fst) $
                      foldMap (\s -> (,) <$> sAssign s <*> pure s) $ pStructs pile
  mkCustomType' pile found ftype

getCustomType :: Pile -> ByteString -> Maybe (Datatype 'Outer)
getCustomType pile bs = do
  (ftype, found) <- find ((==) bs . rType . fst) $
                      foldMap (\s -> (,) <$> sAssign s <*> pure s) $ pStructs pile
  mkCustomType pile found ftype



getType :: Pile -> Record (Maybe ByteString) -> Maybe (Datatype 'Outer)
getType pile rec@(Record _ ns typ isConst _) =
  asum
    [ getBaseType rec
    , getPngType rec
    , do guard $ isNothing ns && not isConst
         getExtraType pile typ
    , do guard $ isNothing ns && not isConst
         getCallbackType pile typ
    , do guard $ isNothing ns && not isConst
         getCustomType pile typ
    , case (typ, ns) of
        ("png_byte"       , Just (1, False)) -> Just . Pointer $ Basic "#{type png_byte}"
        ("png_longjmp_ptr", Nothing        ) -> Just . Pointer $ Basic "CJmpBuf"
        ("png_color_16p"  , Just (1, False)) -> Pointer2 <$> getCustomType' pile "png_color_16"
        ("png_colorp"     , Just (1, False)) -> Pointer2 <$> getCustomType' pile "png_color"
        ("png_color_8p"   , Just (1, False)) -> Pointer2 <$> getCustomType' pile "png_color_8"
        ("png_textp"      , Just (1, False)) -> Pointer2 <$> getCustomType' pile "png_text"
        ("png_timep"      , Just (1, False)) -> Pointer2 <$> getCustomType' pile "png_time"
        _                                    -> Nothing
    ]



filterDef :: Define -> Bool
filterDef (Define typ name) =
  case typ of
    Function -> False
    Constant -> not $ any ($ name) [ (==) "PNG_H"
                                   , (==) "PNG_LIBPNG_VER_STRING"
                                   , (==) "PNG_HEADER_VERSION_STRING"
                                   , BSC.isPrefixOf "png"
                                   , BSC.isPrefixOf "PNG_ARM"
                                   , BSC.isPrefixOf "PNG_MIPS"
                                   , BSC.isPrefixOf "PNG_POWERPC"
                                   , BSC.isPrefixOf "PNG_READ_16_TO_8"
                                   ]

filterDefStr :: Define -> Bool
filterDefStr (Define typ name) =
  case typ of
    Function -> False
    Constant -> any ($ name) [ (==) "PNG_LIBPNG_VER_STRING"
                             , (==) "PNG_HEADER_VERSION_STRING"
                             ]

filterTypedef :: Record ByteString -> Bool
filterTypedef = (/=) "png_libpng_version_1_6_37" . rName



filterExport :: Export ByteString -> Bool
filterExport (Export name _ _ _ _ _) = not $ any ($ name) $ [ (==) "png_err"
                                                            , (==) "png_set_strip_error_numbers"
                                                            , (==) "png_get_io_chunk_name"
                                                            ]



data Import =
       Import
         { iUnsafe :: Bool
         , iName   :: ByteString
         , iArgs   :: [(Maybe ByteString, Datatype 'Outer)]
         , iRet    :: Datatype 'Outer
         }
       deriving Show

mkImport :: Pile -> Export ByteString -> Maybe Import
mkImport pile (Export name _flavor _i ret args _attrs) =
  Import False
    <$> Just name
    <*> do sequence $ (\r -> (,) (rName r) <$> getType pile r) <$> args
    <*> getType pile ret

imports :: Pile -> Maybe [Import]
imports pile = sequence $ mkImport pile <$> filter filterExport (pExports pile)



callbackName :: ByteString -> ByteString
callbackName a = camelCase $ BSC.take (BSC.length a - 4) a



putDatatype :: Datatype a -> Builder
putDatatype (Basic a)               = byteString a
putDatatype (Custom a _)            = byteString $ camelCase a
putDatatype (FunctionPointer a _ _) = "FunPtr " <> byteString (callbackName a)
putDatatype (Pointer a)             = "Ptr "           <> putDatatype a
putDatatype (ConstPointer a)        = "ConstPtr "      <> putDatatype a
putDatatype (RPointer      a)       = "RPtr "          <> putDatatype a
putDatatype (ConstRPointer a)       = "ConstRPtr "     <> putDatatype a
putDatatype (Pointer2      a)       = "Ptr (Ptr "      <> putDatatype a <> ")"
putDatatype (ConstPointer2 a)       = "ConstPtr (Ptr " <> putDatatype a <> ")"
putDatatype (Pointer3      a)       = "Ptr (Ptr (Ptr " <> putDatatype a <> "))"



putType :: Datatype a -> Builder
putType (FunctionPointer name args ret) =
  mconcat
    [ "type ", byteString $ callbackName name, " = "
    , foldMap (\a -> putDatatype a <> " -> ") args
    , "IO " <> if isBasic ret
                 then putDatatype ret
                 else "(" <> putDatatype ret <> ")"
    , "\n\n"
    , "foreign import ccall \"wrapper\"\n"
    , "  mk", byteString $ callbackName name
            , " :: ", byteString $ callbackName name
            , " -> IO (FunPtr ", byteString $ callbackName name, ")"
    ]

putType (Custom name mayArgs) =
  mconcat
    [ "data ", byteString $ camelCase name
    , case mayArgs of
        Nothing             -> mempty
        Just (args, struct) ->
          mconcat
            [ " =\n"
            , "       ", byteString $ camelCase name
            , "\n         { "
            , let f (arg, dt) (isLast, acc) =
                    (,) False $ mconcat
                                  [ byteString $ camelCasePrefixed name arg, " :: ", putDatatype dt
                                  , if isLast
                                      then "\n         } "
                                      else "\n         , "
                                  , acc
                                  ]
              in snd $ foldr f (True, mempty) args
            , "\n\ninstance Storable ", byteString $ camelCase name, " where"
            , "\n  sizeOf _    = #size ", byteString struct
            , "\n  alignment _ = #alignment ", byteString struct, "\n"
            , "\n  peek ptr = ", byteString $ camelCase name
            , let f (isFirst, acc) (arg, _) =
                    (,) False $ mconcat
                                  [ acc
                                  , if isFirst
                                      then "\n               <$> #{peek "
                                      else "\n               <*> #{peek "
                                  , byteString struct, ", ", byteString arg, "} ptr"
                                  ]
              in snd $ foldl' f (True, mempty) args
            , "\n"
            , "\n  poke ptr val = do"
            , flip foldMap args $ \(arg, _) ->
                mconcat
                  [ "\n    #{poke ", byteString struct, ", ", byteString arg, "} ptr $ "
                  , byteString $ camelCasePrefixed name arg, " val"
                  ]
            ]
    ]

putTypes :: [Datatype a] -> Builder
putTypes = foldMap ((<> "\n\n") . putType)



putImport :: Import -> Builder
putImport (Import unsafe name args ret) =
  mconcat
    [ "foreign import ccall "
    , if unsafe
        then "unsafe "
        else mempty
    , "\"", byteString name, "\"\n  ", byteString name
    , case args of
        []                   -> " :: IO " <> if isBasic ret
                                               then putDatatype ret
                                               else "(" <> putDatatype ret <> ")"
        (note1, arg1) : rest ->
          mconcat
            [ "\n    :: ", putDatatype arg1, case note1 of
                                               Just note -> " -- ^ " <> byteString note
                                               Nothing   -> mempty
            , flip foldMap rest $ \(noteX, argX) ->
                mconcat
                  [ "\n    -> "
                  , putDatatype argX, case noteX of
                                          Just note -> " -- ^ " <> byteString note
                                          Nothing   -> mempty
                  ]
            , "\n    -> IO ", if isBasic ret
                                then putDatatype ret
                                else "(" <> putDatatype ret <> ")"
            ]
    ]

putImports :: [Import] -> Builder
putImports = foldMap $ (<> "\n\n") . putImport

putImportsTest :: [Import] -> Builder
putImportsTest = foldMap $ \(Import unsafe name args ret) ->
                   "\n  _ <- report \"" <> byteString name <> "\" " <> byteString name




scavenge :: Pile -> Maybe Builder
scavenge pile = do
  let strs      = toConstants . filter filterDefStr $ pDefs pile
      patterns  = toConstants . filter filterDef $ pDefs pile
  customs   <- customTypes pile
  callbacks <- callbackTypes pile
  imports   <- imports pile
  Just $ mconcat
           [ putPatternsStr strs
           , "\n\n"
           , putPatterns patterns
           , "\n\n"
           , putTypes callbacks
           , "\n"
           , putTypes customs
           , "\n"
           , putImports imports
           ] 



generate :: FilePath -> FilePath -> IO (Either String ())
generate from to = do
  file <- BSL.readFile from
  case pileUp file of
    Left err -> return $ Left err
    Right pile ->
      case scavenge pile of
        Nothing -> return $ Left "Could not scavenge the pile"
        Just res -> do
          BSL.writeFile to . toLazyByteString $
            mconcat
              [ "{-# LANGUAGE OverloadedStrings\n"
              , "           , PatternSynonyms #-}\n"
              , "\n"
              , "module Codec.Image.PNG.Internal.Raw where\n"
              , "\n"
              , "import           Data.Int\n"
              , "import           Data.String (IsString)\n"
              , "import           Data.Word\n"
              , "import           Foreign.C.Types\n"
              , "import           Foreign.Ptr\n"
              , "import           Foreign.Storable\n"
              , "import           System.Posix.Internals\n"
              , "\n"
              , "#include <png.h>\n"
              , "\n"
              , "-- | @const *@\n"
              , "type ConstPtr = Ptr\n"
              , "\n"
              , "-- | @* restrict@\n"
              , "type RPtr = Ptr\n"
              , "\n"
              , "-- | @const * restrict@\n"
              , "type ConstRPtr = Ptr\n"
              , "\n\n"
              , res
              ]
          return $ Right ()

generateTest :: FilePath -> FilePath -> IO (Either String ())
generateTest from to = do
  file <- BSL.readFile from
  case pileUp file of
    Left err -> return $ Left err
    Right pile ->
      case imports pile of
        Nothing -> return $ Left "Could not scavenge imports"
        Just res -> do
          BSL.writeFile to . toLazyByteString $
            mconcat
              [ "module Main where\n"
              , "\n"
              , "import           Dynamic.Report\n"
              , "\n"
              , "import           Codec.Image.PNG.Internal.Raw\n"
              , "\n"
              , "\n"
              , "\n"
              , "-- | Tests whether all of the @foreign import ccall@s refer to correct function names.\n"
              , "--   You do not need to launch the test to know that the names are correctly\n"
              , "--   bound -- that is figured out at compilation time.\n"
              , "main :: IO ()\n"
              , "main = do"
              , putImportsTest res
              , "\n\n"
              , "  putStrLn \"✓ All clear ✓\"\n"
              ]
          return $ Right ()
