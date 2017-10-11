{-
    It takes an argument of type '[Int]'.
-}
doubleAndSum :: [Int] -> Int
doubleAndSum =
    fst .
    foldr (\i (acc, even) -> (acc + nextStep even i, not even)) (0, False)
    where
    nextStep even i
        | even = (uncurry' (+) . (`divMod` 10) . (*2)) i
        | otherwise = i

{-
    It takes an argument of type 'Int'.
-}
myLuhn :: Int -> Bool
myLuhn = (0 ==) . (`mod` 10) . doubleAndSum . (map (read . (:[]))) . show

{-

-}
uncurry' :: (t2 -> t1 -> t) -> (t2, t1) -> t
uncurry' f = \(x, y) -> f x y

{-
    It takes no arguments.
-}
testCC :: [Bool]
testCC =
    map myLuhn [49927398716, 49927398717, 1234567812345678, 1234567812345670]
    -- => [True, False, False, True]