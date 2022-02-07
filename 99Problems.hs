import Data.List (sortOn, group)
import Control.Applicative (Applicative(liftA2))
--1
myLast :: [c] -> c
myLast = head . reverse

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
encode = map (\l -> (length l, head l)) . pack
