{- |
  These solutions are mainly for me to learn recursion through
  functional programming. Therefore these solutions won't be
  most optimal
  -}
-- |1-10 lists

-- |1. Find the last element of a list
-- |It would be easier to just get xs !! len x - 1
myLast :: [a] -> a
myLast [] = error "Cannot find last element of empty list"
myLast (x:[]) = x 
myLast (x:xs) = myLast xs

-- |2. Find the last but one element of a list (second to last)
myButLast :: [a] -> a
myButLast [] = error "Cannot find second to last element of empty list"
myButLast (_:[]) = error "Cannot find second to last element of a list with one element"
myButLast (x:_:[])  = x
myButLast (x:xs) = myButLast xs
                      
-- |3. Find the Kth element of a list. The first element in the list is number 1
elementAt :: [a] -> Integer -> a
elementAt [] _ = error "Cannot find elements in an empty list"
elementAt (x:xs) n
  | n <= 1 = x
  | otherwise = elementAt xs $ n - 1 

-- |4. Find the number of elements in a list
myLength :: (Integral b) => [a] -> b
myLength xs = myLength' 0 xs where
  myLength' accumulator xs = case xs of
    [] -> accumulator
    (x:xs) -> myLength' (accumulator + 1) xs

myLength' :: (Integral b) => [a] -> b
myLength' = foldl (\n _ -> succ n) 0 

-- |5. Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- |6. Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- |7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = case xs of
  [] -> []
  (x:xs') -> flatten x ++ (flatten (List xs'))

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = foldl (++) [] (map flatten' xs)

-- |8. Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress (x:y:xs)
  | x == y     = compress (y:xs)
  | otherwise  = x : (compress (y:xs))
compress xs = xs

compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
  | x == y    = compress ys
  | otherwise = x : compress ys
compress' xs = xs

-- |9. Pack consecutive duplicates of list elements into sublists. If a list
-- |   contains repeated elements they should be placed in separate sublists
pack :: Eq a => [a] -> [[a]]
pack [x] = [[x]]
pack (x:xs) = if elem x (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- |10. Run-length encoding of a list. Use the result of problem P09 to implement
-- |the so-called run-length encoding data compression method. Consecutive duplicates
-- |of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)] 
encode xs = map (\x -> (length x, head x)) (pack (xs))

encode' :: Eq a => [a] -> [(Int, a)]
encode' xs = encode'' (pack xs) where
  encode'' [] = []
  encode'' (x:xs) = (length x, head x) : (encode'' xs)
