module Common where
import Test.QuickCheck (Arbitrary, arbitrary, oneof)

data Tree a = E | T (Tree a) a (Tree a) deriving Show

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
        pure E,
        T <$> arbitrary <*> arbitrary <*> arbitrary
        ]

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
