import Common
-- Ex 2.3
insert :: (Ord a) => Tree a -> a -> Tree a
insert tree x = case (insert' tree) of
    Just t -> t
    Nothing -> tree
    where
    insert' E = pure $ T E x E
    insert' (T left y right)
        | x < y = T <$> (insert' left) <*> (pure y) <*> (pure right)
        | x > y = T left y <$> (insert' right)
        | otherwise = Nothing
