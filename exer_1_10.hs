exerc1 :: [a] -> a
exerc1 (x:[]) = x
exerc1 (_:xs) = exerc1 xs

exerc2 :: [a] -> a
exerc2 (x:[]) = x
exerc2 (_:xs) = exerc1 xs