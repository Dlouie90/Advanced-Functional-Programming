{- 
    The toDigits function takes a number and returns a list of 
    the digits of the numbers in order.
    It takes an argument of type 'Int'. 
-}
toDigits :: Int -> [Int]
toDigits n = map (\x -> read [x]) (show n)

{-
    The doubleEveryOther function takes a list and returns
    the list with every other numbers doubled from right to left.
    It takes an argument of type '[Int]'.
-}
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther ds = zipWith (*) (reverse ds) (cycle [1,2])

{-
    The function sumDigits takes a list of integers
    and returns the sum of the digits of each numbers.
    It takes an argument of type '[Int]'.
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
    valid by using the luhn algorithm and returns true
    if it is valid or false if it is not valid.
    It takes an argument of type 'Int'.
-}
isValid :: Int -> Bool
isValid n = checkSum n `mod` 10 == 0

{-
    testCC is a function that returns a list of true or false depending
    if it is a valid credit card number or not and how many credit cards
    are there in the list. The list of credit card numbers is hardcoded 
    into the function.
    It takes no arguments.
-}
testCC :: [Bool]
testCC = map isValid [79927398713, 79927398714, 4266841244535355, 1234567898765432] 