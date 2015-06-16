{- |
  These solutions are mainly for me to learn recursion through
  functional programming. Therefore these solutions won't be
  most optimal
  -}
-- |11-20 lists

-- |11. Modified run-length encoding.
-- |Modify the result of problem 10 in such a way that if an element has
-- |no duplicates it is simply copied into the result list. Only elements
-- |with duplicates are transferred as (N E) lists.

-- I'm going to reinclude definition of encode here just for readability purposes
import Data.List
data Count a = Multiple Int a | Single a
             deriving Show

encode :: Eq a => [a] -> [(Int, a)] 
encode xs = map (\x -> (length x, head x)) (group (xs))
            
encodeModified :: Eq a => [a] -> [Count a]
encodeModified = map encodeHelper . encode where
  encodeHelper (1, x) = Single x
  encodeHelper (n, x) = Multiple n x
             
encodeModified' xs = [y | x <- group xs,
                      let y =
                            if length x == 1
                            then Single (head x)
                            else Multiple (length x) (head x)]

decodeModified :: (Eq a) => [Count a] -> [a]
decodeModified [] = []
decodeModified (Single a: xs) = a : decodeModified xs
decodeModified (Multiple n a:xs) = (replicate n a) ++ (decodeModified xs)
