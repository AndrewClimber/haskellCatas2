{-
Your task is to make function, which returns the sum of a sequence of integers.

The sequence is defined by 3 non-negative values: begin, end, step.

If begin value is greater than the end, function should returns 0

Examples

sequenceSum (2,2,2) = 2
sequenceSum (2,6,2) = 12 -- 2 + 4 + 6
sequenceSum (1,5,1) = 15 -- 1 + 2 + 3 + 4 + 5
sequenceSum (1,5,3) = 5  -- 1 + 4
This is the first kata in the series:

1) Sum of a sequence (this kata)
2) Sum of a Sequence [Hard-Core Version]
-}

module SequenceSum (sequenceSum) where

-- мой    
sequenceSum :: (Integer,Integer,Integer) -> Integer
sequenceSum (begin, end, step) = sum $ sS (begin, end, step) where 
    sS (begin, end, step)  | begin < end = begin:sS (begin+step, end, step)
                           | otherwise = if (last [begin]) > end then init [begin] else [begin]

-- codewars 
-- Супер!
sequenceSum' (begin, end, step) = sum [begin,(begin+step)..end]

