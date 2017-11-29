-- Problem 1 --

-- The first version uses straightforward recursion.
{-
    This myMap1 function is a tail recursive where function 'f' is applied on 
    each head/first element of the list and then is recursively called 
    on the remaining tail of the list.
    myMap1 _ [] is the edge condition where any function denoted as _
    applied on an empty list will result in an empty list.
-}
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] =  []
myMap1 f (x:xs) =  (f x) : myMap1 f xs


-- What would happen if we reversed the order of the two myMap1 clauses?

-- Is myMap1 tail recursive? Why or why not?
    {- Tail Recursion is the special kind of recursion where the recursive
     call is the last thing in the function. myMap1 is indeed tail recursive,
     because the last thing that happens is the recursive call to the base case  -}


-- The second version implements map in terms of foldr.
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldr (\x accum -> (f x : accum)) [] xs

-- Show how this works by stepping through the execution.
    {-
        MyMap2 takes in two parameters which is a function and a list. Then it uses
        foldr with three parameters, the lambda function, empty list, and the list.
        It uses the last element of the list and the empty list and applies the 
        lambda function.
    -}


-- Problem 2 --

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

myZipWithtest :: [Integer]
myZipWithtest = myZipWith (+) [1, 2, 3] [4, 5, 6]
-- If you use recursion, does the order of the clauses matter? Why or why not?
    {-
        It does not matter.
    -}


-- Problem 3 --

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys

myFoldltest :: Integer
myFoldltest = myFoldr (*) 4 [11, 15, 6]

-- Is this tail recursive. Why or why not?
    {-
        No, because the recursive call is not the last thing that happens.
    -}

{- What is the relationship between the value produced by the base case and the initial function call? 
That is, assume you make a call like this:
> myFoldl fn accInit list and assume that when the base case is reached it returns value
What is the relationship (if any) between value and myFoldl fn accInit list? -}
    {-
        The value of the base case and the initial function call is the same. 
    -}


-- Problem 4 --

-- a.

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f x [] = x
myFoldr f x (y:ys) = f y $ myFoldr f x ys

-- Is this tail recursive. Why or why not?
    {-
        No, because we are computing the accumulator when you return from the recursion.
    -}

-- b.

myFlip :: (t2 -> t1 -> t) -> t1 -> t2 -> t
myFlip f x y = f y x

myFoldr2 :: (a -> b -> b) -> b -> [a] -> b 
myFoldr2 f x [] = x
myFoldr2 f x (y:ys) = myFoldl fp x (reverse (y:ys))
    where fp = myFlip f

-- Problem 5 --

myCycle :: [a] -> [a]
myCycle xs = xs ++ myCycle xs

-- Such a situation would produce an infinite loop in Java. Why doesn’t this lead to an infinite loop in Haskell? 
    {-
        Because Haskell is lazy and does not evaluate expressions that are bounded to variables.
    -}

{- What happens with the following? Explain why.
> cyc12 = myCycle [1,2] - - Does this result in an infinite loop?
> take 5 cyc12 - - Does this result in an infinite loop?


-}

-- Problem 6 --

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

{-
    Given g:: b -> c, f:: a -> b, and h = g `compose` f,
    what is the type of h? (Hint: the type of h is not the same as the type of compose.)

    
-}

-- Problem 7 --

functionPairs1 :: Eq a => (a -> b) -> [a] -> [(a, b)]
functionPairs1 f = \l -> filter (\y -> elem (fst y) l) $ zip l (map f l)

functionPairs2 :: Eq t => (t -> b) -> [t] -> [(t, b)]
functionPairs2 f = \l -> filter (\y -> elem (fst y) l) $ (map (\z -> (z, f z)) l)

-- Problem 8 -- 

while :: state -> (state -> Bool) -> (state -> state) -> state
while state shouldContinue bodyFn
    | shouldContinue state = while (bodyFn state) shouldContinue bodyFn
    | otherwise = state

nSquares:: Int -> [Int]
nSquares n =
    reverse . snd $ -- Get the second element of the final state and reverse it.    
    while (1, [])
    (\(index, _) -> index <= n) -- n is the argument.
    (\(index, list) -> (index + 1, index^2 : list))      