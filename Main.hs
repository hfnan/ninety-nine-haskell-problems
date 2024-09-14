
-- Problem 1
myLast :: [a] -> a 
myLast [] = error "empty list" 
myLast [x] = x 
myLast (_:xs) = myLast xs 


-- Problem 2 
myButLast :: [a] -> a 
myButLast [] = error "empty list" 
myButLast [x] = error "list has only one element" 
myButLast [x, _] = x 
myButLast (_:xs) = myButLast xs 


-- Problem 3 
elementAt :: [a] -> Int -> a 
elementAt [] _ = error "empty list"
elementAt _ 0 = error "zero index"
elementAt (x:xs) i | i == 1 = x 
                   | i > 1 = elementAt xs (i - 1)


-- Problem 4
myLength :: [a] -> Int 
myLength [] = 0 
myLength (_:xs) = 1 + myLength xs 


-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse (x:xs) = myReverse xs ++ [x]


-- Problem 6
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome xs =  xs == myReverse xs


-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List [x]) = flatten x 
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y    = compress (y:xs) 
                  | otherwise = x : compress (y:xs)


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack = pack' []
    where 
        pack' [] [] = []
        pack' (p:ps) [] = [p:ps]
        pack' [] (x:xs) = pack' [x] xs 
        pack' (p:ps) (x:xs) | p == x    = pack' (x:p:ps) xs 
                            | otherwise = (p:ps) : pack' [x] xs 


-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length ys, head ys) | ys <- pack xs]


-- Problem 11
data EncodedList a = Single a | Mutiple Int a deriving Show 

encodeModified :: Eq a => [a] -> [EncodedList a]
encodeModified xs = [if length ys == 1 
                     then Single (head ys) 
                     else Mutiple (length ys) (head ys) | ys <- pack xs]


-- Problem 12
decodeModified :: [EncodedList a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Mutiple n x:xs) = replicate n x ++ decodeModified xs


-- Problem 13
encodeDirect :: Eq a => [a] -> [EncodedList a] 
encodeDirect [] = []
encodeDirect (x:xs) = encodeCount 1 x xs  
    where 
        encodeCount n p [] = [makeEncodedList n p]
        encodeCount n p (x:xs) | p == x    = encodeCount (n + 1) p xs 
                               | otherwise = makeEncodedList n p : encodeCount 1 x xs 
        makeEncodedList 1 p = Single p
        makeEncodedList n p = Mutiple n p


-- Problem 14
dupli :: [a] -> [a]
dupli xs = concat [[x, x] | x <- xs]


-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concat [replicate n x | x <- xs]


-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (i, x) <- zip [1..] xs, i `mod` n /= 0]


-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = splitAt n xs 