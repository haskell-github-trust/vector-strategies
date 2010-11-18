module Data.Vector.Strategies 
	( parVector
	-- * Re-exported for convenience
	, NFData, using
	) where

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies
import qualified Data.Vector as V

-- |Evaluate the elements of a boxed vector in parallel.
--
-- The vector will be divided up into chunks of length less than or
-- equal to the provided chunk size (first argument) and each chunk
-- of elements will be sparked off for evaluation.
-- 
-- Use this along with the "parallel" package's 'using' function:
--
-- @
--    vec `using` (parVector chunkSize)
-- @
--
--  This is not useful on unboxed vectors (will not provide any performance increase)
parVector :: NFData a => Int -> Strategy (V.Vector a)
parVector minChunk vector
 | minChunk <= 0 = parVector 1 vector
 | otherwise = go vector
  where
  go vec =
   let vLen = V.length vec
       half = vLen `div` 2
   in if vLen > minChunk
       then do
         go (V.unsafeSlice 0 half vec)
         go (V.unsafeSlice half (vLen - half) vec)
         return vec
       else evalChunk (vLen-1) >> return vec
   where
   evalChunk (-1) = return vec
   evalChunk i    = rpar (rdeepseq (vec V.! i)) >> evalChunk (i-1)
