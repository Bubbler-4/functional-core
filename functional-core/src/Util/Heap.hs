{-|
    An implementation of Heap API (Appendix A.1, p.270-272)
-}
module Util.Heap where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- | Garbage-collected heap of nodes in each impl
--
-- > (heap, previously freed addresses)
type Heap a = (IntMap a, IntSet)

-- | Unique values to identify the address of each object in the heap
type Addr = Int

-- | Initial empty heap
hInitial :: Heap a
hInitial = (IntMap.empty, IntSet.empty)

-- | Allocate a new address for a new object
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (heap, free) value = case IntSet.minView free of
    -- `free` is empty <=> heap is sequentially full, but `findMax` is faster than `size`
    Nothing -> let key = fst (IntMap.findMax heap) + 1 in ((IntMap.insert key value heap, free), key)
    Just (key, free') -> ((IntMap.insert key value heap, free'), key)

-- | Update a given address to a new object
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (heap, free) key value = (IntMap.insert key value heap, free)

-- | Free the object at the given address
hFree :: Heap a -> Addr -> Heap a
hFree (heap, free) key = (IntMap.delete key heap, IntSet.insert key free)

-- | Get the object at the given address
hLookup :: Heap a -> Addr -> a
hLookup (heap, free) key = case IntMap.lookup key heap of
    Nothing -> error $ "Can't find node " ++ show key ++ " in heap"
    Just value -> value

-- | Addresses of all objects in the heap
hAddresses :: Heap a -> [Addr]
hAddresses (heap, free) = IntMap.keys heap

-- | Number of objects in the heap
hSize :: Heap a -> Int
hSize (heap, free) = IntMap.size heap

-- | A special "null" address, which is different from all allocated addresses
hNull :: Addr
hNull = 0

-- | Is the given address null?
hIsnull :: Addr -> Bool
hIsnull a = a == 0
