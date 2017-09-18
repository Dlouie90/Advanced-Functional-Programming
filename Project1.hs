oddsFrom3 = [3, 5 .. ]
primeDivisors n = [d | d <- takeWhile (\x -> x^2 <= n) primes, n `mod` d == 0]
primes = 2 : [n | n <- oddsFrom3, null (primeDivisors n)]
iSqrt n = floor (sqrt (fromIntegral n))
isASquare 0 = False
isASquare n = (iSqrt n) ^ 2 == n
isPrime n = length(primeDivisors n) == 0
oddComp = [n | n <- oddsFrom3, not(isPrime n)]
filterPrimes g  = [p | p <- takeWhile (< g) primes, (isASquare ( quot ( g - p ) 2 ) )]
goldbach = [g | g <- oddComp, p <- filterPrimes g, k <- [1..], g /= p + 2 * k ^ 2, length(p) == 0]