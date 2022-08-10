{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Finite.StringCounter where

-- base
import GHC.Exts hiding (toList)
import Prelude hiding (lookup)

-- primitive
import Control.Monad.Primitive
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray
import qualified Data.Primitive.MutVar as MutVar

-- hashable
import Data.Hashable

{- Fifth try is the charm.

  Idea:

  1. Encode all data as positive integers.
  2. Save entries in array of type:

    .|-C|W|W|W..|.
     |<--- n --->| = |Ws| + 1
-}

data StringCounter m a = MkStringCounter
  { _array :: !(MutableByteArray (PrimState m))
  , _entries :: !(MutVar.MutVar (PrimState m) [(a, Int)])
  }

new :: PrimMonad m => Int -> m (StringCounter m a)
new n = do
  t <- newByteArray (n * sizeOf (0 :: Int))
  setByteArray t 0 n (-1 :: Int)
  e <- MutVar.newMutVar []
  pure (MkStringCounter t e)
{-# INLINE new #-}

countOn :: forall a m. (PrimMonad m) => (a -> [Int]) -> StringCounter m a -> a -> m ()
countOn group' (MkStringCounter t e) a = do
  go (h `mod` searchSpace)
 where
  ws = group' a
  wl = length ws
  h = hash ws
  searchSpace = size - wl - 2
  size = sizeofMutableByteArray t `quot` sizeOf (0 :: Int)

  go :: Int -> m ()
  go !i = do
    (c, j) <- findNegativeFrom i
    if c < -1
      then do
        findFirstDiffFrom t (j + 1) ws >>= \case
          (-1) -> do
            x <- readByteArray @Int t (j + 1 + wl)
            if x < 0
              then writeByteArray t j (c - 1)
              else go (j + 1 + wl)
          k -> do
            go k
      else
        findFirstPositiveFromTo (j + 1) (j + 2 + wl) >>= \case
          (-1) -> do
            writeAllByteArray t j (-2 : ws)
            MutVar.modifyMutVar' e ((a, j) :)
          k -> do
            go k

  findNegativeFrom !i = do
    v <- readByteArray @Int t i
    if v < 0
      then return (v, i)
      else findNegativeFrom ((i + 1) `mod` searchSpace)

  findFirstPositiveFromTo i k = gof i
   where
    gof !j
      | j == k = return (-1)
      | otherwise = do
        v <- readByteArray @Int t j
        if v < 0 then gof (j + 1) else return j
{-# INLINE countOn #-}

toList :: PrimMonad m => StringCounter m a -> m [(a, Int)]
toList (MkStringCounter t e) = do
  mapM (\(a, i) -> (a,) . (-1 -) <$> readByteArray t i) =<< MutVar.readMutVar e
{-# INLINE toList #-}

writeAllByteArray ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  [Int] ->
  m ()
writeAllByteArray h = go
 where
  go !i = \case
    [] -> return ()
    x : xs -> do
      writeByteArray h i x
      writeAllByteArray h (i + 1) xs
{-# INLINE writeAllByteArray #-}

findFirstDiffFrom ::
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  [Int] ->
  m Int
findFirstDiffFrom h i = \case
  [] -> return (-1)
  x : xs -> do
    v <- readByteArray h i
    if v == x then go (i + 1) xs else return i
 where
  go !j = \case
    [] -> return (-1)
    x : xs -> do
      v <- readByteArray h j
      if v == x then go (j + 1) xs else return j
{-# INLINE findFirstDiffFrom #-}
