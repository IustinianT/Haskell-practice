import System.Win32 (xBUTTON1)
quad :: Num a => a -> a
quad x = y * y where y = x * x

double :: Num a => a -> a
double x = 2 * x

min1 :: Ord a => a -> a -> a
min1 x y = if x < y then x else y

max1 :: Ord a => a -> a -> a
max1 x y = if x > y then x else y

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial x = x * factorial (x-1)

abs1 :: (Ord a, Num a) => a -> a
abs1 x 
    | x < 0 = -x
    | x == 0 = 0
    | x > 0 = x

fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

sumNat :: Int -> Int
-- sumNat x = if x == 0 then 0 else x + sumNat (x-1)

sumNat x
    | x == 0 = 0
    | x > 0 = x + sumNat (x-1)
    | otherwise = error "negative argument"

largest :: Ord a => [a] -> a
largest [] = error "list is empty"
largest (x:xs) = if null xs then x else max1 x (largest xs)

elem' :: Eq a => a -> [a] -> Bool
elem' x l
    | null l = error "list cannot be null"
    | length l == 1 = x == head l
    | otherwise = x == head l || elem' x (tail l)

take' :: (Eq t, Num t) => t -> [a] -> [a]
take' 0 l = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

data Tree a = Leaf a | Branch (Tree a) (Tree a)
height :: Tree a -> Int
height (Leaf _) = 1
height (Branch l r) = 1 + max (height l) (height r)
    where max x y = if x > y then x else y

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

main :: IO()
main = do
        -- print(quad 2)
        -- print(double 2)
        -- print(min1 1 2)
        -- print(max1 1 2)
        -- print(factorial 3)
        -- print(abs1 (-5))
        -- print(fib 3)
        -- print(fib 5)
        -- print(sumNat 2)
        -- print(sumNat 2)
        -- print(largest [1,5,4,6,100,2])
        -- print("cat" == ['c','a','t'])
        -- print("cat" == 'c':['a','t'])
        -- print(elem' 2 [1,5,6,2,3])
        -- print(elem' 2 [1,5,6,3])
        -- print(take' 3 [1,2,3,4,5,6])
        -- print(height (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))))
        print(cartProd [1,2,3] ['a','b','c'])