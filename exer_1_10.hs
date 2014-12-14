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


exerc9 :: (Eq a) => [a] -> [[a]]
exerc9 [] = []
exerc9 r@(x:xs) = (takeWhile (==x) r : exerc9 (dropWhile (==x) xs))