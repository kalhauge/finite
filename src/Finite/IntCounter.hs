{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{- | Code borrowed and changed from  Jaro Reinders' clutter library
 https://github.com/noughtmare/clutter
-}
module Finite.IntCounter (IntCounter, new, countOn, toList) where

import Control.Monad.Primitive
import Data.Bits (Bits (unsafeShiftL))
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import qualified Data.Primitive.MutVar as MutVar
import GHC.Exts hiding (toList)
import Prelude hiding (lookup)

data IntCounter m a
  = MkCounter
      !(MutableByteArray (PrimState m))
      !(MutVar.MutVar (PrimState m) [(a, Int)])

new :: PrimMonad m => Int -> m (IntCounter m a)
new n = do
  a <- newByteArray (n * 2 * sizeOf (0 :: Int))
  setByteArray a 0 (n * 2) (-1 :: Int)
  e <- MutVar.newMutVar []
  pure (MkCounter a e)
{-# INLINE new #-}

countOn :: forall m a. PrimMonad m => (a -> Int) -> IntCounter m a -> a -> m ()
countOn fn (MkCounter t e) a = do
  let i = unsafeShiftL k 1 `mod` n
  slot <- readByteArray t i
  if
      -- slot is empty
      | slot == (-1 :: Int) -> do
        writeByteArray t i k
        writeByteArray t (i + 1) (1 :: Int)
        MutVar.modifyMutVar' e ((a, i + 1) :)
      -- slot is filled
      | slot == k -> do
        v <- readByteArray t (i + 1)
        writeByteArray t (i + 1) (v + 1 :: Int)
      -- wrong slot
      | otherwise -> do
        go ((i + 2) `rem` n)
 where
  k = fn a
  n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
  go :: Int -> m ()
  go !i = do
    slot <- readByteArray t i
    if
        -- slot is empty
        | slot == (-1 :: Int) -> do
          writeByteArray t i k
          writeByteArray t (i + 1) (1 :: Int)
          MutVar.modifyMutVar' e ((a, i + 1) :)
        -- slot is filled
        | slot == k -> do
          v <- readByteArray t (i + 1)
          writeByteArray t (i + 1) (v + 1 :: Int)
        -- wrong slot
        | otherwise -> do
          go ((i + 2) `rem` n)
{-# INLINE countOn #-}

toList :: forall m a. PrimMonad m => IntCounter m a -> m [(a, Int)]
toList (MkCounter t e) =
  mapM (\(a, i) -> (a,) <$> readByteArray t i) =<< MutVar.readMutVar e
{-# INLINE toList #-}

-- toIntList :: forall m a. PrimMonad m => IntCounter m a -> m [(Int, Int)]
-- toIntList (MkCounter t _) =
--   let n = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)
--       go :: [(Int, Int)] -> Int -> m [(Int, Int)]
--       go s !i
--         | i == n = pure s
--         | otherwise = do
--           slot <- readByteArray t i
--           if slot == (-1 :: Int)
--             then do
--               go s (i + 2)
--             else do
--               v <- readByteArray t (i + 1)
--               go ((slot, v) : s) (i + 2)
--    in go [] 0
-- {-# INLINE toIntList #-}
