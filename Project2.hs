{- 
    The toDigits function takes a number and returns a list of 
    the digits of the numbers in order.
    It takes an argument of type 'Int'. 
-}
toDigits :: Int -> [Int]
toDigits n = map (\x -> read [x]) (show n)

{-
    The doubleEveryOther function takes a list, reverses it, and then
    doubles every other digit.
    It takes an argument of type '[Int]'
-}
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther ds = zipWith (*) (reverse ds) (cycle [1,2])

{-
    The function sumDigits takes a list of integers 
    and returns the sum of the numbers.
-}
sumDigits :: [Int] -> Int
sumDigits = sum . concat . map toDigits

{-
    The checkSum function takes in a integer, doubles
    every other integer from right to left, and takes
    the sum of the digits.
    It takes an argument of type 'Int'.
-}
checkSum :: Int -> Int
checkSum = sumDigits . doubleEveryOther . toDigits

{-
    The function isValid checks if the credit card is 
    valid by summing the digits and the doubled digits
    and checking if it is divisible by 10. If it is divisible 
    by 10, it will return true; otherwise it will return false.
    It takes an argument of type 'Int'.
-}
isValid :: Int -> Bool
isValid n = checkSum n `mod` 10 == 0

{-
    testCC is a function that returns a boolean depending
    if it is a valid credit card number or not. The list of
    credit card numbers is hardcoded into the function.
    It takes no arguments.
-}
testCC :: [Bool]
testCC = map isValid [79927398713, 79927398714] 