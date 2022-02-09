import Data.List (sortOn, group, sortBy)
import Control.Applicative (Applicative(liftA2))
import Control.Arrow
import Data.Ord (Down(Down), comparing)
import Data.Tuple (swap)

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