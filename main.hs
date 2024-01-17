square :: Num a => a -> a
square x = x*x

lengthArr :: Num a1 => [a2] -> a1
lengthArr [] = 0
lengthArr (x:xs) = 1 + lengthArr xs

mapFunc :: (t -> a) -> [t] -> [a]
mapFunc f [] = []
mapFunc f (x:xs) = f x : mapFunc f xs

main :: IO()
main = 
    do 
        --putStrLn "Hello World"
        print (square 5)
        print (lengthArr [1,2,3,4,5,6,7])
        print (lengthArr ['1','2','3','4','5','6','7'])
        print (mapFunc square [1..7])
        print (map square [1..7])