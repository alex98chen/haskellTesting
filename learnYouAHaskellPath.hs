data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving(Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath::RoadSystem ->Path
optimalPath roadSystem = 
  let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
  in if sum (map snd bestAPath) <= sum(map snd bestBPath)
    then reverse bestAPath
    else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let 
    priceA = sum $ map snd pathA
    priceB = sum $ map snd pathB
    crossToA = priceB + b + c
    forwardA = priceA + a
    crossToB = priceA + a + c 
    forwardB = priceB + b 
    newPathToA = if forwardA <= crossToA
      then (A,a):pathA
      else (C, c):(B,b):pathB
    newPathToB = if forwardB <= crossToB
      then (B,b):pathB
      else (C, c):(A, a): pathA
  in 
    (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  


main = print $ optimalPath heathrowToLondon
