{-# LANGUAGE BangPatterns #-}

module Parser.Wrap
  ( module Parser.Wrap
  , Result (..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Data.Attoparsec.ByteString.Char8 as Atto ( anyChar, char, peekChar'
                                                          , skipWhile, takeWhile1, scientific )
import           Data.Attoparsec.ByteString.Lazy as Atto hiding (skipWhile, takeWhile1)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import           Data.Foldable (asum)
import           Data.Scientific (toBoundedInteger)



newtype Wrap m a = Wrap { unwrap :: Parser (m -> m, a) }

instance Functor (Wrap m) where
  fmap f (Wrap a) = Wrap $ fmap f <$> a

instance Applicative (Wrap m) where
  pure a = Wrap $ return (id, a)

  Wrap f <*> Wrap a = Wrap $ (\(m, f') (n, a') -> (n . m, f' a')) <$> f <*> a

instance Monad (Wrap m) where
  return = pure

  Wrap f >>= g = Wrap $ do (m, a) <- f
                           (n, b) <- unwrap $ g a
                           return (n . m, b)

instance MonadFail (Wrap m) where
  fail = Wrap . fail

instance Alternative (Wrap m) where
  empty = Wrap empty

  Wrap a <|> Wrap b = Wrap $ a <|> b

instance MonadPlus (Wrap m)



add :: (m -> m) -> Wrap m ()
add f = Wrap $ return (f, ())

wrapping :: Parser a -> Wrap m a
wrapping f = Wrap $ (,) id <$> f

anyChar :: Wrap m Char
anyChar = wrapping Atto.anyChar

char :: Char -> Wrap m Char
char c = wrapping $ Atto.char c

string :: ByteString -> Wrap m ByteString
string bs = wrapping $ Atto.string bs

peekChar :: Wrap m Char
peekChar = wrapping Atto.peekChar'

choice :: [Wrap m a] -> Wrap m a
choice = asum

skipWhile :: (Char -> Bool) -> Wrap m ()
skipWhile f = wrapping $ Atto.skipWhile f

takeWhile1 :: (Char -> Bool) -> Wrap m ByteString
takeWhile1 f = wrapping $ Atto.takeWhile1 f

int :: Wrap m Int
int = wrapping $ do raw <- scientific
                    case toBoundedInteger raw of
                      Nothing -> fail "Not a valid integer"
                      Just n  -> return n

liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)

many1 :: Wrap m a -> Wrap m [a]
many1 p = liftM2' (:) p (many' p)

sepBy :: Wrap m a -> Wrap m b -> Wrap m [a]
sepBy p s = scan `mplus` return []
  where
    scan = liftM2' (:) p ((s >> sepBy1' p s) `mplus` return [])

sepBy1 :: Wrap m a -> Wrap m b -> Wrap m [a]
sepBy1 p s = scan
  where
    scan = liftM2' (:) p ((s >> scan) `mplus` return [])

option :: a -> Wrap m a -> Wrap m a
option x p = p <|> pure x


parse :: Wrap m a -> BSL.ByteString -> Either String (m -> m, a)
parse w = eitherResult . Atto.parse (unwrap w <* endOfInput)


parse' :: Wrap m a -> BSL.ByteString -> Result (m -> m, a)
parse' w = Atto.parse (unwrap w <* endOfInput)
