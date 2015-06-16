map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

reduce' :: (a -> b -> b) -> b -> [a] -> b
reduce' _ accumulator [] = accumulator
reduce' f accumulator (x:xs) = f x (reduce' f accumulator xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate (x:xs)
  | predicate x = x : filter' predicate xs
  | otherwise = filter' predicate xs

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > (maximum' xs) = x
  | otherwise = maximum' xs

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n- 1) x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = (n == x) || (elem' n xs)

elem'' :: Eq a => a -> [a] -> Bool
elem'' n [] = False
elem'' n (x:xs)
  | n == x = True
  | otherwise = elem'' x xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [ a | a <- xs, a <= x ]
      bigger  = quicksort [ a | a <- xs, a  > x ]
  in smaller ++ [x] ++ bigger

