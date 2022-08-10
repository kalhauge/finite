{-# LANGUAGE BangPatterns #-}

module Finite.Class (Finite (..), ascii) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.List as List

{- | A finite piece of data, which we can convert into a list of positive intergers.
  The following should hold true:

 laws:
 - a == b <=> group a == group b
-}
class Finite a where
  group :: a -> [Int]

instance Finite Int where
  group a
    | a <= 0 = [0, negate a]
    | otherwise = [a]
  {-# INLINE group #-}

instance Finite a => Finite [a] where
  group = concatMap group
  {-# INLINE group #-}

-- | If your bytestring only contain ascii charactors we can compress them a little more.
ascii :: B.ByteString -> [Int]
ascii !bs
  | B.null bs = []
  | otherwise =
    ( List.foldl'
        (\a r' -> a `unsafeShiftL` 7 .|. idx r')
        0
        [0 .. x - 1]
    ) :
    ascii (B.drop x bs)
 where
  x = min 9 t
  idx = fromIntegral . B.index bs
  t = B.length bs
{-# INLINE ascii #-}
