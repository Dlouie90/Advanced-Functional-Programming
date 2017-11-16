-- Problem 1 --

-- The first version uses straightforward recursion.
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] =  []
myMap1 f (x:xs) =  (f x) : myMap1 f xs

-- What would happen if we reversed the order of the two myMap1 clauses?

-- Is myMap1 tail recursive? Why or why not?
    {- Tail Recursion is the special kind of recursion where the recursive
     call is the last thing in the function.  -}
-- The second version implements map in terms of foldr.
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldr (\x accum -> (f x : accum)) [] xs

-- Problem 2 --

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

-- If you use recursion, does the order of the clauses matter? Why or why not?

-- Problem 3 --

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys

-- Is this tail recursive. Why or why not?

{- What is the relationship between the value produced by the base case and the initial function call? 
That is, assume you make a call like this:
> myFoldl fn accInit listand assume that when the base case is reached it returns value
What is the relationship (if any) between value and myFoldl fn accInit list? -}


-- Problem 4 --

-- a.

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f x [] = x
myFoldr f x (y:ys) = f y $ myFoldr f x ys

-- b.

myFlip :: (t2 -> t1 -> t) -> t1 -> t2 -> t
myFlip f x y = f y x

myFoldr2 :: (a -> b -> b) -> b -> [a] -> b 
myFoldr2 f x [] = x
myFoldr2 f x (y:ys) = myFoldl fp x (reverse (y:ys))
    where fp = myFlip f

-- Problem 5 --

myCycle :: [a] -> [a]
myCycle (x:xs) = (x:xs) ++ myCycle (x:xs)

-- Problem 6 --

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Problem 7 --

functionPairs1 :: Eq a => (a -> b) -> [a] -> [(a, b)]
functionPairs1 f = \l -> filter (\y -> elem (fst y) l) $ zip l (map f l)

functionPairs2 :: Eq t => (t -> b) -> [t] -> [(t, b)]
functionPairs2 f = \l -> filter (\y -> elem (fst y) l) $ (map (\z -> (z, f z)) l)

-- Problem 8 -- 

--while :: state -> (state -> Bool) -> (state -> state) -> (state -> result) -> result
while state cond body build = 
    if (cond state)
        then while (body state) cond body build
        else return (build state)

nSquares:: Int -> [Int] 
nSquares n = 
    while (1, []) 
        (\(index, _) -> index <= n) -- n is the nSquares argument.
        (\(index, list) -> (index + 1, index^2 : list)) -- bodyFn
        (reverse . snd) -- Extract the second element of
                         -- the tuple and reverse it.
        