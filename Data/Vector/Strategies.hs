module Data.Vector.Strategies 
	( parVector
	-- * Re-exported for convenience
	, NFData, using
	) where

import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies
import Control.Monad
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
--    vec \``using`\` (`parVector` chunkSize)
-- @
--
-- 'parVector' can not provide any benefits (read: no parallelism) for unboxed vectors!
parVector :: NFData a => Int -> Strategy (V.Vector a)
parVector n = liftM V.fromList . parListChunk n rdeepseq . V.toList

instance NFData a => NFData (V.Vector a) where
  rnf = rnf . V.toList
