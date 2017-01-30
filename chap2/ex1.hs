-- Ex 2.1
suffixes :: [a] -> [[a]]
suffixes [] = []:[]
suffixes list@(_:xs) = list:suffixes xs
