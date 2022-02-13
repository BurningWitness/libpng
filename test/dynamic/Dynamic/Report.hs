{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dynamic.Report where

import           Data.Int
import           Data.Proxy
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr



report' :: String -> IO a -> IO a
report' name action = do
  result <- action
  putStrLn $ "✓ " <> name
  return result



class Def a where
  def :: a

instance Def Word8 where
  def = 0

instance Def Word16 where
  def = 0

instance Def Word32 where
  def = 0

instance Def Word64 where
  def = 0

instance Def Int8 where
  def = 0

instance Def Int16 where
  def = 0

instance Def Int32 where
  def = 0

instance Def Int64 where
  def = 0

instance Def Float where
  def = 0

instance Def Double where
  def = 0

instance Def (Ptr a) where
  def = nullPtr

instance Def (FunPtr a) where
  def = nullFunPtr



class Report a z | a -> z where
  report :: String -> a -> IO z

instance Report (IO z) z where
  report = report'

instance Def a => Report (a -> IO z) z where
  report name f = report' name $ f def

instance (Def a, Report (b -> c) z) => Report (a -> b -> c) z where
  report name f = report name $ f def
