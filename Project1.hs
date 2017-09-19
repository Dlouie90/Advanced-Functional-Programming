{- 
	The 'goldbachCheck' function generates a list of tuples that passes the condition: 
	If there is any prime number and twice a square that can make the Integer parameter.
	It takes one argument of type 'Int'.
-}

goldbachCheck:: Int -> [(Int, Int)]
goldbachCheck g  = [(p, iSqrt ( quot ( g - p ) 2 )) | 
                     p <- takeWhile (< g) primes, isASquare ( quot ( g - p ) 2 ) ]
{- 
	The 'iSqrt' function takes the square root of the number and then 
	take the floor of the number (round down) and returns the number.
	It takes one argument of type 'Int'.
-}
iSqrt:: Int -> Int
iSqrt n = floor (sqrt (fromIntegral n))

{- 
	The 'isASquare' function takes an integer, uses the 'iSqrt' function to get the square root of the number
	and squares the number. Then, the function checks if the evaluated number is the same as the original number
	in the parameter and if it is the same, the function returns True and if it is not the same, then the function returns false.
	It takes one argument of type 'Int'.
-}
isASquare:: Int -> Bool
-- isASquare 0 = False
isASquare n = (iSqrt n) ^ 2 == n

{- 
	The 'isPrime' function takes an integer and uses the 'primeDivisor' function to 
	It takes one argument of type 'Int'.
-}
isPrime:: Int -> Bool
isPrime n = null(primeDivisors n)

{- 
	The 'oddComp' is a list of odd composite(non-prime) integers 
	that was generated by using the 'oddsFrom3' list and checking each element 
	if it is not prime using the 'isPrime' function.
-}
oddComp:: [Int]
oddComp = [n | n <- oddsFrom3, not(isPrime n)]

{- 
	The 'oddsFrom3' list was generated using list comprehension to get all the odd number to infinity.
-}
oddsFrom3:: [Int]
oddsFrom3 = [3, 5 .. ]

{- 
	The 'primeDivisors' function creates a list of integers that are prime and a divisor of the parameter that was given.
	It takes one argument of type 'Int'.
-}
primeDivisors:: Int -> [Int]
primeDivisors n = [d | d <- takeWhile (\x -> x^2 <= n) primes, n `mod` d == 0]

{- 
	The 'primes' list is a list generated by going through each element from 'oddsFrom3' 
	and checking if they do not have any prime divisors.
-}
primes:: [Int]
primes = 2 : [n | n <- oddsFrom3, null (primeDivisors n)]

{- 
	The 'goldbach' list creates a list of the numbers that fail goldbach's conjecture 
	and cannot produce a prime number p and a square number that can sum up to the number that is given.
	GoldbachCEx means goldbach counter example.
-}
goldbachCEx:: [Int]
goldbachCEx = [g | g <- oddComp, null (goldbachCheck g)]