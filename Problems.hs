module Problems (Problem, problemList) where

import Data.List
import Data.Function
import Data.Char

-- Convention - keep title below 15 characters
data Problem = Problem {probTitle :: String, probText :: String, probAns :: String}

instance Show Problem where
    show = probTitle

problemList :: [Problem]
problemList = [euler1,
               euler2,
               euler3,
               euler4,
               euler5,
               euler6,
               euler7,
               euler8,
               euler9,
               euler10,
               euler11,
               euler12,
               euler14,
               euler15,
               euler16,
               euler18,
               noFours,
               fizzBuzz
               ]

-- ===============
-- Utility functions, used in multiple problems
-- ===============

-- Calculate the fibonacci numbers less than 'top' with 
-- seeds 'x' and 'y'
-- fibonacci 5 0 1 -> [1, 2, 3]
fibonacci :: (Num a, Ord a) => a -> a -> a -> [a]
fibonacci top x y = 
    let next xs = xs ++ [(last xs) + (last (init xs))]
    in  tail(tail((last(takeWhile ((<top).last) (iterate next [x,y])))))

-- Calculate the first 'top' fibonacci numbers with 
-- seeds 'x' and 'y'
-- fibonacci' 5 0 1 -> [1, 2, 3, 5, 8]
fibonacci' :: (Num a) => Int -> a -> a -> [a]
fibonacci' top x y = 
    let next xs = xs ++ [(last xs) + (last (init xs))]
    in  tail(tail((last(take (top+1) (iterate next [x,y])))))

-- The fibonacci sequence, elegantly (but slow!)
fib x y = x : y : (zipWith (+) (fib x y) (tail(fib x y)))
-- Again, but fast (apparently since we didn't use arbitrary seeds)
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

-- The primes, very elegantly done in one line
primes = nubBy (((>1) .) . gcd) [2..]

-- Factors of a number, as quickly as possible
factors :: Integer -> [Integer]
factors n =
    let bottom = filter (\x -> (n `mod` x) == 0) (takeWhile (\x -> (fromIntegral x) <= sqrt (fromIntegral n)) [1..])
    in union bottom (reverse (map (div n) bottom))

-- Factorial
factorial 1 = 1
factorial n = n * (factorial (n-1))

-- ===============
-- Actual Problems
-- ===============

fizzBuzz = Problem "FizzBuzz"
    "Print the numbers 1-100, except that whenever a number is divisible by 3 print Fizz, and whenever it is divisible by 5 print Buzz. For 15, print FizzBuzz."
    (intercalate "\n" $ take 30 fizzBuzz_f)

-- I heard about a solution that doesn't involve checking modulos - how
-- fun!
fizzBuzz_f :: [String]
fizzBuzz_f = zipWith pairUp [1..] fizzbuzzes where
    fizzes = cycle [[], [], "Fizz"]
    buzzes = cycle [[], [], [], [], "Buzz"]
    fizzbuzzes = zipWith (++) fizzes buzzes
    pairUp :: Int -> String -> String
    pairUp x [] = show x
    pairUp _ s = s

noFours = Problem "No Fours"
    "In China, number '4' is not good because it is has the same pronunciation as 'death' in Chinese. so there is a new number system which we may make 4 disappear in the current decimal system. like: 1,2,3,5,6,7.......13,15.......23,25.......33,35....39,50........ here 5 is 4 in decimal system, and 15 is 13..... so write a function, input a positive number and output should be like this:\n1 -> 1;\n2 -> 2;\n5 -> 4;\n4 -> illegal;\n15 -> 13;"
    (noFours_f 50)

noFours_f :: Int -> String
noFours_f x = intercalate "\n" $ map (\n -> show n ++ " -> " ++ getNum n) [1..x] where
    maybeToResult (Just s) = show s
    maybeToResult (Nothing) = "illegal"
    filtered = filter (not . any (=='4')) $ map show [1..]
    getNum n = maybeToResult . fmap (+1) . elemIndex (show n) . take n $ filtered



euler18 = Problem "PE 18"
    "Find the maximum total from top to bottom of the triangle below:\n--\n75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
    (show euler18_f)

euler18_f =
    -- bigPair takes a list, then returns a shorter
    -- list with the largest numbers in it, taken
    -- pairwise.
    -- bigPair [8, 5, 9, 3] -> [8, 9, 9]
    let bigPair (x:y:[]) = (max x y):[]
        bigPair (x:y:xs) = (max x y):(bigPair (y:xs))
    in head (foldr1 (\xs acc -> (zipWith (+) xs (bigPair acc))) [
      [75]
    , [95, 64]
    , [17, 47, 82]
    , [18, 35, 87, 10]
    , [20, 04, 82, 47, 65]
    , [19, 01, 23, 75, 03, 34]
    , [88, 02, 77, 73, 07, 63, 67]
    , [99, 65, 04, 28, 06, 16, 70, 92]
    , [41, 41, 26, 56, 83, 40, 80, 70, 33]
    , [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
    , [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
    , [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
    , [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
    , [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
    , [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
   ])

euler16 = Problem "PE 16"
    "What is the sum of the digits of the number 2^1000?"
    (show $ euler16_f (2^1000))

euler16_f :: Integer -> Int
euler16_f x = sum (map digitToInt (show x))

euler15 = Problem "PE 15"
    "Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.  How many routes are there through a 20x20 grid?"
    (show $ euler15_f 20 20)

-- Using math, fast
euler15_f :: Integer -> Integer -> Integer
euler15_f rows cols = (factorial (rows + cols)) `div` ((factorial rows) * (factorial cols))

-- Using recursion, slow
euler15_f' :: Integer -> Integer -> Integer
euler15_f' 0 _ = 1
euler15_f' _ 0 = 1
euler15_f' rows cols = (euler15_f' (rows - 1) cols) + (euler15_f' rows (cols - 1))

euler14 = Problem "PE 14"
    "The following iterative sequence is defined for the set of positive integers:\n--\nn  n/2 (n is even)\nn  3n + 1 (n is odd)\n--\nUsing the rule above and starting with 13, we generate the following sequence:\n--\n13  40  20  10  5  16  8  4  2  1\n--\nIt can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1. Which starting number, under one million, produces the longest chain? NOTE: Once the chain starts the terms are allowed to go above one million."
    (show $ euler14_f 1000000)

euler14_f n =
    let chainLength 1 = 1
        chainLength x = 1 + (chainLength (next x))
        next x = if (odd x) then (3*x+1) else (x `div` 2)
    in (fst . head) (sortBy ((flip compare) `on` snd) (map (\x -> (x, chainLength x)) [1..n]))

euler12 = Problem "PE 12"
    "What is the value of the first triangle number to have over five hundred divisors?"
    (show $ euler12_f 500)

euler12_f n =
    let triangles = map (\n -> (n*(n+1) `div` 2)) [1..]
        xs = map (length . factors) triangles
    in triangles !! (head (findIndices (>n) xs))

euler11 = Problem "PE 11"
    "What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 2020 grid?\n08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
    (show $ euler11_f)

euler11_f =
    let nums =
            [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08]
            , [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00]
            , [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65]
            , [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91]
            , [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80]
            , [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50]
            , [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70]
            , [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21]
            , [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72]
            , [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95]
            , [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92]
            , [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57]
            , [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58]
            , [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40]
            , [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66]
            , [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69]
            , [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36]
            , [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16]
            , [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54]
            , [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]
        maxProd xs@(_:_:_:_:_) = maximum (zipWith4 (\a b c d -> a*b*c*d) xs (tail xs) (tail . tail $ xs) (tail . tail . tail $ xs))
        maxProd xs = 0
        shifted xs = transpose ((\(a,b) -> b) (mapAccumL (\x y -> (x+1, (replicate x 0) ++ y)) 0 xs))
    in maximum . concat $ [map maxProd nums, map maxProd (transpose nums), map maxProd (shifted nums), map maxProd (shifted . transpose $ nums)]

euler10 = Problem "PE 10"
    "Find the sum of all the primes below two million."
    (show $ euler10_f 2000000)

euler10_f y = sum . (takeWhile (<y)) $ primes

euler9 = Problem "PE 9"
    "There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."
    (show euler9_f)

euler9_f =
    let trip = [(x,y,1000-x-y) | x <- [1..1000], y<- [x..1000], x^2 + y^2 == (1000-x-y)^2]
    in head (map (\(x,y,z) -> x*y*z) trip)

euler8 = Problem "PE 8"
    "Find the greatest product of five consecutive digits in the 1000-digit number\n7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    (show $ euler8_f 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

euler8_f x =
    let list = zipWith5 (\a b c d e -> [a,b,c,d,e]) (show x) (tail . show $ x) (tail . tail . show $ x) (tail . tail . tail . show $ x) (tail . tail . tail . tail . show $ x)
        digits = last (sortBy (compare `on` (product . (map digitToInt))) (list))
    in product (map digitToInt digits)

euler7 = Problem "PE 7"
    "Find the 10001 prime."
    (show $ euler7_f 10001)

-- Naive method
euler7_f' x =
    let nextPrime [] = 2
        nextPrime [2] = 3
        nextPrime xs = last (takeWhile (condition xs) [((last xs) + 1)..]) + 1
        condition xs b = any (\x -> (b `mod` x) == 0) xs
    in head ((iterate (\xs -> (nextPrime xs):xs) [2]) !! x)

-- Faster method
euler7_f x = primes !! x

euler6 = Problem "PE 6"
    "Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum."
    (show $ euler6_f' [1..100])

-- Naieve approach
euler6_f :: [Integer] -> Integer
euler6_f xs = (sum xs)^2 - sum (map (^2) xs)

-- Much faster
euler6_f' :: [Integer] -> Integer
euler6_f' xs =
    let cross = [x*y | x <- xs, y <- xs, x /= y]
    in sum cross

euler5 = Problem "PE 5"
    "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?"
    (show $ euler5_f [1..20])
      
-- Naive approach - just check all number until we get there
euler5_f' :: (Integral a) => [a] -> a
euler5_f' xs =
    let condition (x:[]) z = (mod z x) == 0
        condition zs z = ((mod z (head zs)) == 0) && (condition (tail zs) z)
        smaller = takeWhile ((==False).(condition xs)) [1..]
    in 1 + (last smaller)

-- Better approach, by finding gccds and lcms (which are
-- already implemented, but which I reimplemented for
-- practice)
euler5_f :: [Integer] -> Integer
euler5_f xs =
    let factors x ns = filter (\y -> (mod x y == 0)) ns
        commonFactors x y = (factors (max x y) (factors (min x y) (take (fromIntegral (min x y)) [1..])))
        gcf x y = if length (commonFactors x y) == 0 then 1 else maximum (commonFactors x y)
        lcmul (x:[]) = x
        lcmul (x:y:[]) = div ((toInteger x)*(toInteger y)) (gcf x y)
        lcmul (x:xs) = lcmul (x:(lcmul xs):[])
    in lcmul xs

euler4 = Problem "PE 4"
    "Find the largest palindrome made from the product of two 3-digit numbers."
    (show euler4_f)

euler4_f :: Int
euler4_f = 
    let products = [x*y | x <- [100..999], y <- [100..999]]
        palindromes = filter (\x -> if ((show x) == (reverse . show $ x)) then True else False) products
    in maximum palindromes

euler3 = Problem "PE 3"
    "What is the largest prime factor of the number 600851475143?"
    (show $ euler3_f 600851475143)

euler3_f :: Int -> Int
euler3_f x =
    let firstFactor z [] = error "no factors!"
        firstFactor 1 xs = 1
        firstFactor z xs = 
            if ( (mod z (head xs)) == 0) then (head xs)
            else firstFactor z (tail xs)
        largestFactor z = div z (firstFactor z [2..])
        factorList = iterate largestFactor x
    in  factorList !! ((subtract 1) . head $ (elemIndices 1 factorList))

euler2 = Problem "PE 2"
    "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
    (show $ euler2_f 4000000)

euler2_f :: (Integral a) => a -> a
euler2_f top = 
    let evens = filter even (fibonacci top 0 1)
    in sum evens

euler1 = Problem "PE 1"
    "Find the sum of all the multiples of 3 or 5 below 1000."
    (show $ euler1_f [3,5] 1000)

euler1_f :: (Integral a) => [a] -> a -> a
euler1_f multiples top = 
    let condition [] a = False
        condition (x:rest) a = (a `mod` x == 0) 
            || condition rest a
        all = filter (condition multiples) [1..]
    in  sum . takeWhile (<top) $ all
