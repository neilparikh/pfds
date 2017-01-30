main :: IO ()
main = do
    print $ suffixes ([1, 2, 3, 4] :: [Int])
    -- x <- getLine
    -- print $ member (T (T E 2 E) 3 (T E 4 E) :: Tree Int) (read x) Nothing
    -- y <- getLine
    -- print $ insert (T (T E 2 E) 3 (T E 4 E) :: Tree Int) (read y)
