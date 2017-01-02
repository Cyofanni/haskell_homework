import Control.Applicative

newtype ZipList' a = Z [a] deriving Show


instance Functor ZipList' where
    fmap g (Z xs) = Z (fmap g xs)


instance Applicative ZipList' where
    pure x = Z (repeat x)
    Z fs <*> Z xs = Z (zipWith id fs xs)
 


list_3_a :: [a] -> [a] -> [a] -> [[a]]
list_3_a xs ys zs = [[x, y, z] | x <- xs, y <- ys, z <- zs]

--app_list_3_a xs yx zs = 

list_3_b :: [a] -> [a] -> [a] -> [[a]]
list_3_b [] [] [] = []
list_3_b (x:xs) (y:ys) (z:zs) = [[x] ++ [y] ++ [z]] ++ (list_3_b xs ys zs)

list_3_c :: [[Int]] -> Int -> [[Int]]
list_3_c [] _ = []
list_3_c (x:xs) (n) = [map (+n) x] ++ (list_3_c xs (n + 1)) 

