import Common
-- Ex 2.4
insert :: (Ord a) => Tree a -> a -> Tree a
insert tree x = case (insert' tree Nothing) of
    Just t -> t
    Nothing -> tree
    where
    insert' E Nothing = pure $ T E x E
    insert' E (Just y)
        | x == y = Nothing
        | otherwise = pure $ T E x E
    insert' (T left y right) possibleMatch
        | x < y = T <$> (insert' left possibleMatch) <*> (pure y) <*> (pure right)
        | otherwise = T left y <$> (insert' right (Just y))
