module Allocator where

import Data.List
import Data.Array.IO
import Control.Monad (replicateM)

data PoolAllocator a = PoolAllocator
    { blocks :: [IOArray Int a]
    , blockSize :: Int
    , blockCount :: Int
    , freeBlocks :: [Int]
    }

createPoolAllocator :: Int -> Int -> IO (PoolAllocator a)
createPoolAllocator blockSize blockCount
  | blockSize <= 0 || blockCount <= 0 = error "incorrect blockSize or blockCount"
  | otherwise = do
        blocks <- replicateM blockCount $ newArray (0, blockSize - 1) undefined
        let freeIndices = [0..blockCount - 1]
        return $ PoolAllocator { blocks = blocks
                               , blockSize = blockSize
                               , blockCount = blockCount
                               , freeBlocks = freeIndices
                               }
        
allocateBlock :: PoolAllocator a -> a -> IO (PoolAllocator a)
allocateBlock allocator value
    | null (freeBlocks allocator) = error "No free blocks available"
    | otherwise = do
        let idx = head (freeBlocks allocator)
            rest = tail (freeBlocks allocator)
            blockArray = blocks allocator !! idx
        writeArray blockArray 0 value      
        return $ allocator { freeBlocks = rest }

freeBlock :: PoolAllocator a -> Int -> IO (PoolAllocator a)
freeBlock allocator blockIdx
    | blockIdx < 0 || blockIdx >= blockCount allocator = error "Invalid block index"
    | otherwise = 
        return $ allocator { freeBlocks = blockIdx : freeBlocks allocator }

