import Data.List ( group, groupBy, permutations, sortOn, groupBy, find )
import Control.Applicative (Applicative(liftA2))
import Control.Arrow
import Data.Ord (Down(Down))
import Data.Tuple (swap)
import Data.Functor
import System.Random ( randomRIO )
import Control.Monad ( replicateM )
import Data.Maybe (fromJust)

--1
myLast :: [c] -> c
myLast = last

--2
myButLast :: [c] -> c
myButLast = head . tail . reverse

--3
elementAt :: [c] -> Int -> c
elementAt = flip $ (head .) . drop . subtract 1

--4
myLength :: (Foldable t, Num b1) => t b2 -> b1
myLength = foldl (const . (+1)) 0

--5
myReverse :: [b] -> [b]
myReverse = foldl (flip (:)) []

--6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = liftA2 ($) (==) reverse

isPalindrome2 :: Eq a => [a] -> Bool
isPalindrome2 = (==) <*> reverse

isPalindrome3 :: Eq a => [a] -> Bool
isPalindrome3 = reverse >>= (==)

--7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List x) = concatMap flatten x

--8
compress :: Eq b => [b] -> [b]
compress = concatMap (take 1) . group

compress2 :: Eq a => [a] -> [a]
compress2 = map head . group

--9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = takeWhile (head xs ==) xs : pack (dropWhile (head xs ==) xs)

--10
encode :: Eq b => [b] -> [(Int, b)]
encode = map (length &&& head) . pack

encode2 :: Eq b => [b] -> [(Int, b)]
encode2 = map (length &&& head) . group


--11
data ListItem a = Multiple Int a | Single a
    deriving (Show, Eq)

fromTuple :: (Int, a) -> ListItem a
fromTuple (1,b) = Single b
fromTuple (a,b) = Multiple a b

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map fromTuple . encode

--12
decodeModified :: Foldable t => t (ListItem b) -> [b]
decodeModified = concatMap decodeList
    where decodeList (Single b) = [b]
          decodeList (Multiple a b) = replicate a b

toTuple :: ListItem a -> (Int, a)
toTuple (Single x)     = (1, x)
toTuple (Multiple n x) = (n, x)

decodeModified2 :: [ListItem a] -> [a]
decodeModified2 = concatMap (uncurry replicate . toTuple)

--13
increment :: ListItem a -> ListItem a
increment = fromTuple . first (+1) . toTuple

value :: ListItem a -> a
value = snd . toTuple

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = reverse . foldl countValues []
    where countValues b a
            | null b || value (head b) /= a = Single a : b
            | otherwise = increment (head b) : tail b

--14
dupli :: [a] -> [a]
dupli = flip repli 2

--15
repli :: [b] -> Int -> [b]
repli xs n = foldr (\a b -> replicate n a ++ b) [] xs

--16
dropNth :: [a] -> Int -> [a]
dropNth xs n = take (n-1) xs ++ drop n xs

dropNth2 :: [a] -> Int -> [a] --point-free madness.......(un)curry
dropNth2 = flip $ (fmap . fmap) (uncurry (++)) $ curry $ (uncurry take . first (subtract 1)) &&& uncurry drop

--17
split :: [a] -> Int -> ([a], [a])
split = flip $ curry (uncurry take &&& uncurry drop)


--18
slice :: [a] -> Int -> Int -> [a]
slice xs i j = fst $ split (snd $ split xs (i-1)) (j-i+1)

slice2 :: [a] -> Int -> Int -> [a]
slice2 xs i j = [ x | (k,x) <- zip [1..] xs, i<= k && k <= j]

--19
rotate :: [a] -> Int -> [a]
rotate xs n = drop modLength xs ++ take modLength xs
    where modLength = mod n (length xs)

rotate2 :: [a] -> Int -> [a] --point free madness...................cur.........ry
rotate2 = fmap (uncurry (++) . (uncurry drop &&& uncurry take)) <$> curry (uncurry mod . swap . first length &&& fst)

--20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (head $ take n xs, dropNth xs n)

removeAt2 :: Int -> [c] -> (c, [c])
removeAt2 = curry (head . uncurry take &&& uncurry (flip dropNth))

--21
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs i = uncurry ((++) <$> (++[e])) $ split xs (i-1)

--22
range :: Enum a => a -> a -> [a]
range i j = [i..j]

--23
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs r = map (xs!!) <$> replicateM r (randomRIO (0,length xs - 1::Int))

--24
rangeRndSelect :: Int -> Int -> IO [Int]
rangeRndSelect n x = rndSelect [1..x] n

--25
rndPermu :: [a] -> IO [a]
rndPermu xs = rndSelect (permutations xs) 1 <&> head

--26
combinations :: Int -> [a] -> [[a]] --Permutations are not considered
combinations n xs = filter ((n==) . length) (powerList xs)
    where powerList [] = [[]]
          powerList (x:xs) = powerList xs ++ map (x:) (powerList xs)

--27
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ] ++ [ (ys,x:zs) | (ys,zs) <- combination n xs ]

myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (n:ns) xs = [ g:gs | (g,rs) <- combination n xs,  gs <- myGroup ns rs ]

--28
lsort :: Foldable t => [t a] -> [t a]
lsort = sortOn length

lfsort :: Foldable t => [t a] -> [t a]
lfsort xs = concat $ sortOn length $ groupBy (\x y -> length x == length y) $ sortOn length xs

--31
isPrime :: Integral a => a -> Bool
isPrime n = length [d | d <- [1..floor $ sqrt (fromIntegral n)], n `mod` d == 0] == 1

--32
myGcd :: Integral t => t -> t -> t
myGcd a 0 = abs a
myGcd a b = myGcd b (a `mod` b)

--33
coprime :: Integral a => a -> a -> Bool
coprime a b = 1 == myGcd a b

--34
totient :: Integral a => a -> Int
totient n = length [ t | t <- [1..n-1], coprime t n ]

--35
primeFactors :: Int -> [Int]
primeFactors n = concatMap (uncurry $ flip replicate) $ primeFactorsMult n

primes :: [Int]
primes = filterPrimes [2..]
    where filterPrimes (p:xs) = p:filterPrimes [ x | x <- xs, mod x p /= 0 ]

--36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult x = [(n, m n) | n <- takeWhile (<= x) primes, m n /= 0]
    where m n = maximum [ e | e <- [0..n], x `mod` n^e == 0]

--37
phi :: Int -> Int
phi n = foldl (\a (p,e) -> a * (p-1) * p ^ (e-1)) 1 $ primeFactorsMult n

phi2 :: Int -> Int
phi2 n = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult n]

--39
primesR :: Int -> Int -> [Int]
primesR i j = [ p | p <- take j primes, i <= p && p <= j ]

--40
goldbach :: Int -> (Int, Int)
goldbach n = head [ (p, n-p) | p <- primes, isPrime (n-p) ]

--41

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList i j = [ goldbach e | e <- [i..j], even e ]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' i j l = filter (\(a,b) -> a > l && b > l) $ goldbachList i j 

