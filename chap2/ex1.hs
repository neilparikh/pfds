import Data.List (tails)
import Test.QuickCheck

main = quickCheck prop_suffixes_eq_tails

-- Ex 2.1
suffixes :: [a] -> [[a]]
suffixes [] = []:[]
suffixes list@(_:xs) = list:suffixes xs

prop_suffixes_eq_tails :: [Int] -> Bool
prop_suffixes_eq_tails list = suffixes list == tails list
