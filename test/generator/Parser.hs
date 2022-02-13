{-# LANGUAGE DeriveFunctor
           , OverloadedStrings #-}

module Parser where

import           Parser.Wrap

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set



data Defineable = Constant
                | Function
                  deriving Show

data Define = Define Defineable ByteString
              deriving Show

data Record name = Record
                     { rName     :: name
                     , rPointers :: Maybe (Int, Bool)
                     , rType     :: ByteString
                     , rIsConst  :: Bool
                     , rIsStruct :: Bool
                     }
                   deriving (Show, Functor)

data Struct name = Struct
                     { sName     :: Maybe name
                     , sRecords  :: [Record ByteString]
                     , sAssign   :: [Record (Maybe ByteString)]
                     }
                   deriving Show

data Callback name = Callback
                       { cName   :: name
                       , cReturn :: ByteString
                       , cArgs   :: [Record (Maybe ByteString)]
                       }
                     deriving Show

data Flavor = Regular
            | WithAttribute
            | FloatingPoint
            | FixedPoint
            | Removed
              deriving Show

data Export name = Export
                     { eName   :: name
                     , eFlavor :: Flavor
                     , eId     :: Int
                     , eReturn :: Record (Maybe ByteString)
                     , eArgs   :: [Record (Maybe ByteString)]
                     , eAttrs  :: [ByteString]
                     }
                   deriving Show


data Pile =
       Pile
         { pDefs      :: [Define]
         , pStructs   :: [Struct ByteString]
         , pTypedefs  :: [Record ByteString]
         , pCallbacks :: [Callback ByteString]
         , pExports   :: [Export ByteString]
         }
       deriving Show

defPile :: Pile
defPile = Pile [] [] [] [] []

newDef :: Define -> Pile -> Pile
newDef def pile = pile { pDefs = pDefs pile <> [def]}

newStruct :: Struct ByteString -> Pile -> Pile
newStruct st pile = pile { pStructs = pStructs pile <> [st] }

newTypedef :: Record ByteString -> Pile -> Pile
newTypedef def pile = pile { pTypedefs = pTypedefs pile <> [def] }

newCallback :: Callback ByteString -> Pile -> Pile
newCallback cb pile = pile { pCallbacks = pCallbacks pile <> [cb] }

newExport :: Export ByteString -> Pile -> Pile
newExport ex pile = pile { pExports = pExports pile <> [ex] }



name :: Wrap m ByteString
name = takeWhile1 $ \a -> isAlphaNum a || a == '_'

identifier :: Wrap m (Maybe (Int, Bool), ByteString)
identifier =
  choice
    [ do stars <- sepBy1 (void $ char '*') space
         space
         isRestricted <- option False $ do _ <- string "PNG_RESTRICT"
                                           space
                                           return True
         (,) (Just (length stars, isRestricted)) <$> name
    , do ide <- name
         choice
           [ do _ <- char '*'
                return (Just (1, False), ide)
           , do _ <- char '['
                skipWhile (/= ']')
                _ <- char ']'
                return (Just (1, False), ide)
           , return (Nothing, ide)
           ]
    ]

comments :: Wrap m ()
comments = do
  _ <- string "/*"
  comments'

comments' :: Wrap m ()
comments' = do
  _ <- skipWhile (/= '*')
  let inner = void (string "*/") <|> do _ <- anyChar
                                        _ <- skipWhile (/= '*')
                                        inner
  inner


space :: Wrap m ()
space = space' $ skipWhile isSpace

linespace :: Wrap m ()
linespace = space' $ skipWhile (== ' ')

chompspace :: Wrap m ()
chompspace = space' . void $ takeWhile1 isSpace

space' :: Wrap m () -> Wrap m ()
space' f = do
  f
  choice
    [ do _ <- string "\\\n"
         space
    , do comments
         space
    , return ()
    ]

next :: Wrap Pile ()
next =
  choice
    [ do '\n' <- anyChar
         space
    , do _ <- skipWhile $ not . flip elem ['#', '\\', '/', '\n', '"']
         choice
           [ do _ <- string "\\\n"
                next
           , do _ <- char '\n'
                space
           , do '/' <- peekChar
                choice [ do comments
                            next
                       , do _ <- anyChar
                            next
                       ]
           , do preprocessor
                space
           , do _ <- char '"'
                skipWhile (/= '"')
                _ <- char '"'
                next
           ]
    ]

nextRec :: Wrap Pile ()
nextRec = do
  choice
    [ do comments
         option () nextRec
    , do preprocessor
         option () nextRec
    , do chompspace
         option () nextRec
    ]



preprocessor :: Wrap Pile ()
preprocessor = do
  _ <- char '#'
  linespace
  choice
    [ do _ <- string "define"
         linespace
         defname <- name
         linespace
         choice [ do _ <- char '('
                     add . newDef $ Define Function defname
                , add . newDef $ Define Constant defname
                ]
         next
    , do next
         preprocessor <|> return ()
    ]

record :: Wrap Pile (Record ByteString)
record = do
  typ <- choice
           [ string "unsigned char"
           , string "unsigned int"
           , name
           ]
  space
  (n, ide) <- identifier
  return $ Record ide n typ False False

recordMay :: Wrap Pile (Record (Maybe ByteString))
recordMay = choice
              [ do (isConst, isStruct) <- option (False, False) $ do
                                            _  <- string "const"
                                            space
                                            is <- option False $ do _ <- string "struct"
                                                                    space
                                                                    return True
                                            return (True, is)
                   Record ide n typ _ _ <- record
                   return $ Record (Just ide) n typ isConst isStruct
              , do (n, typ) <- identifier
                   return $ Record Nothing n typ False False
              ]

records :: Wrap Pile [Record ByteString]
records = do
  _ <- char '{'
  next
  rs <- sepBy record $ do _ <- char ';'
                          nextRec
  next
  _ <- char '}'
  return rs

recordMays :: Wrap Pile [Record (Maybe ByteString)]
recordMays = sepBy1 recordMay $ do space
                                   _ <- char ','
                                   space

typedef :: Wrap Pile ()
typedef = do
  _ <- string "typedef"
  space
  choice
    [ do _ <- string "struct"
         space
         choice
           [ do defname <- option Nothing $ do dn <- name
                                               space
                                               return $ Just dn
                rs <- option [] $ do rs <- records
                                     space
                                     return rs
                as <- recordMays
                space
                _ <- char ';'
                add . newStruct $ Struct defname rs as
           ]

    , do _ <- string "PNG_CALLBACK("
         space
         ret <- name
         space
         _ <- char ','
         space
         (Just (1, False), ptr) <- identifier
         space
         _ <- char ','
         space
         _ <- char '('
         space
         args <- recordMays
         space
         _ <- char ')'
         space
         _ <- char ')'
         space
         _ <- char ';'
         add . newCallback $ Callback ptr ret args

    , do isConst <- option False $ do _ <- string "const"
                                      space
                                      return True
         space
         typ <- name
         space
         (ns, ide) <- identifier
         space
         _ <- char ';'
         add . newTypedef $ Record ide ns typ isConst False
    ]

hasSemicolon :: Flavor -> Bool
hasSemicolon FloatingPoint = False
hasSemicolon FixedPoint    = False
hasSemicolon Removed       = False
hasSemicolon _             = True

export :: Wrap Pile ()
export = do
  flavor <- choice
              [ do _ <- string "PNG_EXPORT"
                   choice [ WithAttribute <$ char 'A'
                          , return Regular
                          ]
              , FloatingPoint <$ string "PNG_FP_EXPORT"
              , FixedPoint    <$ string "PNG_FIXED_EXPORT"
              , Removed       <$ string "PNG_REMOVED"
              ]
  space
  _ <- char '('
  space
  i <- int
  space
  _ <- char ','
  space
  ret <- recordMay
  space
  _ <- char ','
  space
  funname <- name
  space
  _ <- char ','
  space
  _ <- char '('
  space
  rs <- ([] <$ string "void") <|> recordMays
  space
  _ <- char ')'
  space
  attrs <- option [] $ do _ <- char ','
                          space
                          attrs <- sepBy name space
                          space
                          return attrs
  _ <- char ')'
  space
  when (hasSemicolon flavor) $ do
    void $ char ';'
  add . newExport $ Export funname flavor i ret rs attrs



pileUp :: BSL.ByteString -> Either String Pile
pileUp = fmap (\(f, ()) -> f defPile) . parse loop
  where
    loop :: Wrap Pile ()
    loop =
      choice
        [ do choice
               [ chompspace
               , comments
               , void $ string "extern \"C\" {"
               ]
             loop
        , do preprocessor
             loop
        , do typedef
             loop
        , do export
             loop
        , do _ <- string "PNG_FUNCTION"
             next
             loop
        , do _ <- string "PNG_EXPORT_LAST_ORDINAL"
             space
             _ <- char '('
             space
             i <- int
             space
             _ <- char ')'
             space
             _ <- char ';'
             loop
        , do _ <- char '}'
             loop
        , return ()
        ]
