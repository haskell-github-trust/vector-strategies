module Data.Vector.Strategies 
	( parVector
	-- * Re-exported for convenience
	, NFData, using
	) where

import Control.DeepSeq (NFData(..))
import Control.Parallel.Strategies
import Control.Monad
import qualified Data.Vector.Generic as V
import qualified Data.Vector as VB

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
parVector :: V.Vector v a => NFData a => Int -> Strategy (v a)
parVector n = liftM V.fromList . parListChunk n rdeepseq . V.toList
