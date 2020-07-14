-- Exercise 1
myLast :: [a] -> a
myLast l = last l

-- Exercise 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (x:[]) = error "Only one element in the list"
myButLast l = last (init l)

-- Exercise 3
elementAt :: [a] -> Int -> a
elementAt l 0 = error "Index parameter should be > 1"
elementAt l n = l !! (n - 1) 

-- Exercise 4
myLength :: [a] -> Int
myLength [] = 0
myLength (h:t) = 1 + myLength t

-- Exercise 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = myReverse (t) ++ [h]

-- Exercise 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == myReverse l

-- Exercise 7
data NestedList a = Elem a | List[NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (h:t)) = flatten h ++ flatten (List t)

-- Exercise 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (h:t) = h : compress [ x | x <- t, x /= h ]

-- Exercise 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (h:t) = 
    if h `elem` head (pack t)
    then (h : head (pack t)) : (tail (pack t))
    else [h] : pack t

-- Exercise 10
encode :: Eq a => [a] -> [(Int,a)]
encode l = 
    let x = pack l
    in [ (length k, head k) | k <- x ]