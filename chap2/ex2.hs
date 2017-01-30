import Common
-- Ex 2.2
member :: Ord a => Tree a -> a -> Maybe a -> Bool
member E _ Nothing = False
member E x (Just possible) = x == possible
member (T left y right) x maybePossible
    | x < y = member left x maybePossible
    | otherwise = member right x (Just y)
