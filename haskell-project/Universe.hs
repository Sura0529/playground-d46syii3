{-# LANGUAGE CPP #-}
module Universe(sumBuiltinUsed,countAllStars) where

-- This is really more than unsafe.
-- Better alternative welcome!
import Data.IORef
import System.IO.Unsafe
import Prelude hiding (sum)
import qualified Data.Foldable (sum)
sumBuiltinUsed = unsafePerformIO (newIORef False)
sum :: (Num n, Foldable t) => t n -> n
sum = unsafePerformIO (writeIORef sumBuiltinUsed True) `seq` Data.Foldable.sum

#include "universe.hs"

