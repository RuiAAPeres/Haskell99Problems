exerc1 :: [a] -> a
exerc1 (x:[]) = x
exerc1 (_:xs) = exerc1 xs

exerc2 :: [a] -> a
exerc2 [] = error "No end for empty lists!"
exerc2 (x:[]) = error "No end for empty lists!"
exerc2 (x:xs:[]) = x
exerc2 (x:xs) = exerc2 xs

exerc2' :: [a] -> a
exerc2' = (head . tail . reverse)

exerc3 :: [a] -> Int -> a
exerc3 (x:_) 1 = x
exerc3 (x:xs) i = exerc3 xs (i-1)

exerc3' :: [a] -> Int -> a
exerc3' x i    = x !! (i-1)