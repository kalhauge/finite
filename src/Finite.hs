{-# LANGUAGE RankNTypes #-}

module Finite (new, count, countOn, toList, module Class) where

import Finite.Class as Class
import qualified Finite.IntCounter as IntCounter
import qualified Finite.StringCounter as StringCounter

-- primitive

import Control.Monad (liftM2)
import Control.Monad.Primitive

data Counter m a
  = MkCounter
      {-# UNPACK #-} !(IntCounter.IntCounter m a)
      {-# UNPACK #-} !(StringCounter.StringCounter m a)

new :: forall m a. PrimMonad m => Int -> Int -> m (Counter m a)
new is ss = do
  i <- IntCounter.new is
  s <- StringCounter.new ss
  pure $ MkCounter i s
{-# INLINE new #-}

countOn :: forall m a. PrimMonad m => (a -> [Int]) -> Counter m a -> a -> m ()
countOn fn (MkCounter i s) a = case fn a of
  [x] -> IntCounter.countOn (const x) i a
  xs -> StringCounter.countOn (const xs) s a
{-# INLINE countOn #-}

count :: forall m a. (Class.Finite a, PrimMonad m) => Counter m a -> a -> m ()
count = countOn Class.group
{-# INLINE count #-}

toList :: forall m a. (Class.Finite a, PrimMonad m) => Counter m a -> m [(a, Int)]
toList (MkCounter i s) = liftM2 (++) (IntCounter.toList i) (StringCounter.toList s)
{-# INLINE toList #-}
