module Common where
import Test.QuickCheck (Arbitrary, arbitrary, frequency, choose, Gen, sized)
import System.Random (Random)
import Control.Monad (liftM3)

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

-- from http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html
instance (Ord a, Enum a, Bounded a, Random a, Arbitrary a) => Arbitrary (Tree a)  where
    arbitrary = sized $ gen minBound maxBound where
        gen :: (Ord a, Random a, Bounded a, Enum a) => a -> a -> Int -> Gen (Tree a)
        gen min max size
            | size == 0 = return E
            | max <= (succ . succ . succ) minBound = return E
            | min >= pred maxBound = return E
            | min >= (pred . pred . pred) max  = return E
        gen min max size = do
            elt <- choose (min, max)
            frequency [ (1, return E),
                        (6, liftM3 T (gen min (pred elt) (size `div` 2))
                        (return elt) (gen (succ elt) max (size `div` 2))) ]

member :: Ord a => Tree a -> a -> Bool
member E _ = False
member (T left y right) x
    | x < y = member left x
    | x > y = member right x
    | otherwise = True

insert :: (Ord a) => Tree a -> a -> Tree a
insert E x = T E x E
insert tree@(T left y right) x
    | x < y = T (insert left x) y right
    | x > y = T left y (insert right x)
    | otherwise = tree
