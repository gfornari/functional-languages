-- toDigits

toDigitsList :: Integer -> [Integer] -> [Integer]
toDigitsList n list = do
    if n `div` 10 <= 0 then
        [n] ++ list
    else do
        let newList = [(n `mod` 10)] ++ list
        toDigitsList (n `div` 10) newList

toDigits :: Integer -> [Integer]
toDigits n = do
    case compare n 0 of
        GT -> toDigitsList n []
        _ -> []

-- toDigitsRev

toDigitsListRev :: Integer -> [Integer] -> [Integer]
toDigitsListRev n list = do
    if n `div` 10 <= 0 then
        list ++ [n]
    else do
        let newList = list ++ [(n `mod` 10)]
        toDigitsListRev (n `div` 10) newList 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = do
    case compare n 0 of
        GT -> toDigitsListRev n []
        _ -> []

-- doubleEveryOther
{-
doubleEveryOtherList :: [Integer] -> [Integer] -> [Integer]
doubleEveryOtherList list newList = do
    -- using take instead of head because it works with empty list
    let headList = take 1 list
    headList ++ newList

    -- using drop instead of tail because it works with empty list
    let tailList = drop 1 list

    -- check if list is empty
    if null tailList then
        newList
    else do
        [((head tailList) * 2)] ++ newList
        doubleEveryOtherList tailList newList
-}

doubleEveryOtherList :: [Integer] -> [Integer]
-- from http://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
doubleEveryOtherList l = zipWith l (cycle [id,(*2)])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse (doubleEveryOtherList (reverse list)) -- doubleEveryOtherList (reverse list) []

-- sumDigits

sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum) (map (toDigits) list))

-- validate

validate :: Integer -> Bool
validate creditCardNumber = do
    let checkSum = sumDigits (doubleEveryOther (toDigits creditCardNumber))
    (checkSum `mod` 10) == 0

main = do
    putStrLn "toDigits and toDigitsRev"
    putStrLn (show (toDigits 234))
    putStrLn (show (toDigitsRev 234))
    putStrLn (show (toDigits 5))
    putStrLn (show (toDigitsRev 5))
    putStrLn (show (toDigits (-17)))
    putStrLn (show (toDigitsRev (-17)))
    putStrLn (show (toDigits 0))
    putStrLn (show (toDigitsRev 0))
    
    putStrLn "doubleEveryOther"
    putStrLn (show (doubleEveryOther [1,2,3,4,5]))
    putStrLn (show (doubleEveryOther [1,2,3,4,5,6]))
    putStrLn (show (doubleEveryOther []))

    putStrLn "sumDigits"
    putStrLn (show (sumDigits [1,2,3,4,5,6]))
    putStrLn (show (sumDigits [1,23,4,5,6]))
    putStrLn (show (sumDigits [1,2345,6]))

    putStrLn "validate"
    putStrLn (show (validate 5442742557784776))
    putStrLn (show (validate 5442742557783776))
    putStrLn (show (validate 4012888888881881))
    putStrLn (show (validate 4012888888881882))