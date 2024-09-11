
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
