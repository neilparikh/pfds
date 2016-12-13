main :: IO ()
main = do
    print $ suffixes ([1, 2, 3, 4] :: [Int])
    -- x <- getLine
    -- print $ member (T (T E 2 E) 3 (T E 4 E) :: Tree Int) (read x) Nothing
    -- y <- getLine
    -- print $ insert (T (T E 2 E) 3 (T E 4 E) :: Tree Int) (read y)

suffixes :: [a] -> [[a]]
suffixes [] = []:[]
suffixes list@(_:xs) = list:suffixes xs

data Tree a = E | T (Tree a) a (Tree a) deriving Show

member :: Ord a => Tree a -> a -> Maybe a -> Bool
member E _ Nothing = False
member E x (Just possible) = x == possible
member (T left y right) x maybePossible
    | x < y = member left x maybePossible
    | otherwise = member right x (Just y)

insert :: (Ord a) => Tree a -> a -> Tree a
insert E x = T E x E
insert tree@(T left y right) x
    | x < y = T (insert left x) y right
    | x > y = T left y (insert right x)
    | otherwise = tree

insert2 :: (Ord a) => Tree a -> a -> Tree a
insert2 tree x = case (insert2' tree) of
    Just t -> t
    Nothing -> tree
    where
    insert2' E = pure $ T E x E
    insert2' (T left y right)
        | x < y = T <$> (insert2' left) <*> (pure y) <*> (pure right)
        | x > y = T left y <$> (insert2' right)
        | otherwise = Nothing
