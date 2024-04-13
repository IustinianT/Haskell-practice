{-# LANGUAGE TemplateHaskell #-}
import Data.Typeable
import GHC.Real ()
import Prelude hiding (iterate)

square :: Num a => a -> a
square x = x*x

lengthArr :: Num a1 => [a2] -> a1
lengthArr [] = 0
lengthArr (x:xs) = 1 + lengthArr xs

mapFunc :: (t -> a) -> [t] -> [a]
mapFunc f [] = []
mapFunc f (x:xs) = f x : mapFunc f xs

toThePower4 n = square (square n)
                where square n = n * n

quad :: Num a => a -> a
quad x = y*y where y = x*x

double :: Num a => a -> a
double x = 2*x

min :: Ord a => a -> a -> a
min x y = if x > y then y else x

fact :: Int -> Int
fact x 
    | x==0 = 1
    | x>0 = x*fact(x-1)
    | otherwise = error "invalid input for factorial"

mult x y = if x == 0 then 0 else x*y
infinity = infinity + 1

fib n
    | n == 0 = 1
    | n == 1 = 1
    | n > 1 = fib(n-1) + fib(n-2)
    | n < 0 = error "Negative argument for fib"

-- roots :: Float -> Float -> Float -> (Float, Float)
-- roots a b c 
--     | a == 0 = error "Not a quadratic equation"
--     | d < 0 = error "This equation has complex roots"
--     | otherwise (((-b)+r)/e, ((-b)-r)/e) 
--     where {
--         d = b*b - 4*a*c;
--         r = sqrt d;
--         e = 2*a
--     }

iterate 0 f x = x
iterate n f x = iterate (n-1) f (f x)

append [] y z = y == z
append (x:xs) y (z:zs) = append xs y zs

main :: IO()
main = do
        --putStrLn "Hello World"
        --print (square 5)
        --print (lengthArr [1,2,3,4,5,6,7])
        --print (lengthArr ['1','2','3','4','5','6','7'])
        --print (mapFunc square [1..7])
        --print (map square [1..7])
        --print (foldr (-) 0 [1..5])
        --print (foldl (-) 0 [1..5])
        -- let x = 3; f z = z + x in let x = True in f 6
        -- print (quad 2)
        -- print (fact 5)
        -- print (typeOf fact)
        -- print (mult 0 infinity)
        -- print (mult infinity 0)
        -- print (fib 6)
        -- print (roots 1 (-2) 1)
        -- print (iterate 5 double 1)
        print (append [1,2,3] [4,5] [1,2,3,4,5])
