calc :: (Num a, Read a) => String -> a
calc xs = head (foldl stacker [] (words xs))
  where
    stacker (x:y:xs) "+" = (x+y):xs
    stacker (x:y:xs) "-" = (x-y):xs
    stacker (x:y:xs) "*" = (x*y):xs
    stacker xs y = (read y):xs


main = print (calc "5 6 7 + *")
