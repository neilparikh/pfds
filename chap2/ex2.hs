import qualified Common
import Common (Tree(..))
import Test.QuickCheck

main = quickCheck prop_member_should_match_reference_impl

-- Ex 2.2

member :: Ord a => Tree a -> a -> Maybe a -> Bool
member E _ Nothing = False
member E x (Just possible) = x == possible
member (T left y right) x maybePossible
    | x < y = member left x maybePossible
    | otherwise = member right x (Just y)

prop_member_should_match_reference_impl :: Tree Int -> Int -> Bool
prop_member_should_match_reference_impl tree x = member tree x Nothing == Common.member tree x
