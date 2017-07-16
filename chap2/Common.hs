module Common where
import Test.QuickCheck (Arbitrary, arbitrary, frequency, choose, Gen)
import System.Random (Random)
import Control.Monad (liftM3)

data Tree a = E | T (Tree a) a (Tree a) deriving (Show, Eq)

-- from http://www.seas.upenn.edu/~cis552/12fa/lectures/stub/BST.html
instance (Ord a, Bounded a, Random a, Num a, Arbitrary a) => Arbitrary (Tree a)  where
    arbitrary = gen 0 100 where
        gen :: (Ord a, Num a, Random a) => a -> a -> Gen (Tree a)
        gen min max | (max - min) <= 3 = return E
        gen min max = do
            elt <- choose (min, max)
            frequency [ (1, return E),
                        (6, liftM3 T (gen min (elt - 1))
                        (return elt) (gen (elt + 1) max)) ]

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
