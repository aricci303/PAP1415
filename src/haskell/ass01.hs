-- Corso PAP 2014-2015 - ISI-LM - UNIBO
-- Sorgenti soluzioni Assignment #1 


data Elem = Dot | Star

countStar :: [Elem] -> Int
countStar [] = 0
countStar (Star:xs) = 1 + countStar xs
countStar (_:xs) = countStar xs

--

swapSeq :: [Elem] -> [Elem]
swapSeq [] = []
swapSeq (Star:xs) = Dot : swapSeq xs
swapSeq (Dot:xs) = Star : swapSeq xs

--

zipSeq :: [Elem] -> [Elem]
zipSeq [] = []
zipSeq (Dot:xs) = (Dot : zipSeq (skipDots xs))
zipSeq (Star:xs) = Star:zipSeq xs 

skipDots :: [Elem] -> [Elem]
skipDots (Dot:xs) = skipDots xs
skipDots xs = xs

--

printableSeq :: [Elem] -> [Char]
printableSeq [] = []
printableSeq (Dot:xs) = '.': printableSeq xs  
printableSeq (Star:xs) = '*': printableSeq xs  

--

maxStarSeq :: [Elem] -> Int
maxStarSeq xs = maxStarSeq2 xs (-1) 0
maxStarSeq2 [] m c = max m c
maxStarSeq2 (Dot:xs) m c = maxStarSeq2 xs (max m c) 0
maxStarSeq2 (Star:xs) m c = maxStarSeq2 xs m (c + 1)

--

matchSeq :: [Elem] -> [Elem] -> Bool
matchSeq xs ys = matchSeq2 (dropDots xs) (dropDots ys)

matchSeq2 :: [Elem] -> [Elem] -> Bool
matchSeq2 [] xs = onlyDots xs
matchSeq2 xs [] = onlyDots xs
matchSeq2 (Star:xs) (Star:ys) = matchSeq2 xs ys
matchSeq2 (Star:_) (Dot:_) = False
matchSeq2 (Dot:_) (Star:_) = False
matchSeq2 (Dot:xs) (Dot:ys) = matchSeq xs ys

dropDots :: [Elem] -> [Elem]
dropDots [] = []
dropDots (Dot:xs) = dropDots xs
dropDots (Star:xs) = Star:xs

onlyDots :: [Elem] -> Bool
onlyDots [] = True
onlyDots (Dot:xs) = onlyDots xs
onlyDots (Star:_) = False

--

type StarSeqInfo = (Int,Int)  -- Lunghezza/Posizione di una sequenza di Star
type Occur = (Int,[Int]) -- Occorrenze di sequenze di Star (Lunghezza, elenco posizioni)

occ :: [Elem] -> [Occur]
occ xs = reduceSeq (parseSeq xs 0 1) []

parseSeq :: [Elem] -> Int -> Int -> [StarSeqInfo]
-- data una lista di Elem, computa una lista di StarSeqInfo che riporta per ogni sequenza di
-- Star la sua la lunghezza e posizione
parseSeq [] len pos
    | len /= 0 = [(len,pos)]
    | otherwise = []
parseSeq (Star:xs) len pos = parseSeq xs (len+1) pos
parseSeq (Dot:xs) len pos
    | len /= 0 = (len,pos) : parseSeq xs 0 (pos+len+1)
    | otherwise = parseSeq xs len (pos+1)

reduceSeq :: [StarSeqInfo] -> [Occur] -> [Occur]
-- data una lista di informazioni sulle sequenze, le aggrega in modo da avere 
-- un solo elemento per ogni sequenza di una data lunghezza, riportando l’elenco delle
-- posizioni in cui la sequenza compare
reduceSeq [] xs = xs
reduceSeq (x:xs) ys = reduceSeq xs (addSeq x ys)

addSeq :: StarSeqInfo -> [Occur] -> [Occur]
addSeq (x,p) [] = [(x,[p])]
addSeq (x,p) ((x1,pl):xs)  
    | x == x1   = (x,pl++[p]):xs
    | otherwise = (x1,pl) : addSeq (x,p) xs

--


data BSTree a = Nil | Node a (BSTree a)(BSTree a)

countStarInTree :: BSTree Elem -> Int
countStarInTree Nil = 0
countStarInTree (Node Star l r) = 1 + countStarInTree l + countStarInTree r
countStarInTree (Node Dot l r) = countStarInTree l + countStarInTree r

--

pathTree :: BSTree Elem -> Int
pathTree Nil = 0
pathTree (Node Dot _ _) = 0
pathTree (Node Star l r) = 1 + maxv (pathTree l) (pathTree r)

maxv x y | x > y = x
         | otherwise = y




