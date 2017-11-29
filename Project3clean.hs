-- Problem 1 --

{-
    This myMap1 function is a tail recursive where function 'f' is applied on 
    each head/first element of the list and then is recursively called 
    on the remaining tail of the list.
    myMap1 _ [] is the base condition where any function denoted as _
    applied on an empty list will result in an empty list.
-}
myMap1 :: (a -> b) -> [a] -> [b]
myMap1 _ [] =  []
myMap1 f (x:xs) =  (f x) : myMap1 f xs


{-
    MyMap2 takes in two parameters which is a function and a list. Then it uses
    foldr with three parameters, the lambda function, empty list, and the list.
    It uses the last element of the list and the empty list and applies the 
    lambda function.
-}
myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f xs = foldr (\x accum -> (f x : accum)) [] xs


-- Problem 2 --

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

myZipWithtest :: [Integer]
myZipWithtest = myZipWith (+) [1, 2, 3] [4, 5, 6]


-- Problem 3 --

{-
    myFoldl takes in a function, value, and a list and calls
    myFoldl again, but with the second parameter as the function
    applied to the value and the head of the list and the third
    parameter as the rest of the list. This is tail recursive.
-}
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys

myFoldltest :: Integer
myFoldltest = myFoldl (*) 4 [11, 15, 6]


-- Problem 4 --

{- a.
    myFoldr takes in the same parameters as myFold, but the execution is
    different. myFoldr is trying to apply the function with the head of the
    list and the value, but the value is being used for the recursive call.
    This is not tail recursive.
-}
myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f x [] = x
myFoldr f x (y:ys) = f y $ myFoldr f x ys

myFoldrtest = myFoldr (/) 3 []

{- b.
    This version of myFoldr2 uses a myflip function which flips the parameters's order
    and applies the function. myFoldr2 uses myFoldl with the first param as myFlip f,
    because foldr uses the head of the list as the first parameter where as the foldl uses
    the value (x) as the first parameters. The second parameter is still the value and the
    third parameter uses a reversed list since we are using foldl to foldr.
-}
myFlip :: (t2 -> t1 -> t) -> t1 -> t2 -> t
myFlip f x y = f y x

myFoldr2 :: (a -> b -> b) -> b -> [a] -> b 
myFoldr2 f x [] = x
myFoldr2 f x (y:ys) = myFoldl fp x (reverse (y:ys))
    where fp = myFlip f

myFoldr2test = myFoldr (/) 2 [8, 12, 24, 4]


-- Problem 5 --
{-
    myCycle takes a list and appends the same list with the same elements
    in the same order infinitely.
-}
myCycle :: [a] -> [a]
myCycle xs = xs ++ myCycle xs

myCycletest = take 5 (myCycle [1, 2])


-- Problem 6 --
{-
    compose is the same function as . where it takes the result from one function
    and uses it as input for the other.
-}
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

--composetest = (compose abs (-1)) 50


-- Problem 7 --
{-
    functionPairs takes in a function as a list and creates a lambda function
    that 
-}
functionPairs1 :: Eq a => (a -> b) -> [a] -> [(a, b)]
functionPairs1 f = \l -> filter (\y -> elem (fst y) l) $ zip l (map f l)

functionPairs2 :: Eq t => (t -> b) -> [t] -> [(t, b)]
functionPairs2 f = \l -> filter (\y -> elem (fst y) l) $ (map (\z -> (z, f z)) l)

f1 x = x^2
sqrPairs = functionPairs1 f1
sqrPairstest = sqrPairs [1 .. 10]

-- f2 x = x^3 - x^2 + x
-- cubeMinusX2PlusXPairs = functionPairs2 f2
-- cubeMinusX2PlusXPairstest = cubeMinusX2PlusXPairs [1 .. 9]


-- Problem 8 -- 
{-
    The while function is similar to a java while function where as long as the given
    condition is true (shoudlContinue state) then while will recursively call.
-}
while :: state -> (state -> Bool) -> (state -> state) -> state
while state shouldContinue bodyFn
    | shouldContinue state = while (bodyFn state) shouldContinue bodyFn
    | otherwise = state

{-
    The state is (1, []). The shouldContinue part is the second parameter with
    the test index <= n. The bodyFn is the third parameter which appends the current
    index squared to the head of the list and increases the index by 1.
-}
nSquares:: Int -> [Int]
nSquares n =
    reverse . snd $ -- Get the second element of the final state and reverse it.    
    while (1, [])
    (\(index, _) -> index <= n) -- n is the argument.
    (\(index, list) -> (index + 1, index^2 : list))
    
whiletest = nSquares 15