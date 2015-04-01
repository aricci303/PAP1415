-- PAP a.a. 2014-2015 - Lab 01

isPresent :: Int -> [Int] -> Bool
isPresent _ [] = False
isPresent x (y:xs) 
	| x == y = True
	| otherwise = isPresent x xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

removeAll :: (Eq a) => [a] -> a -> [a]
removeAll [] _ = []
removeAll (x:xs) y 
	| x == y = removeAll xs y
	| otherwise = x : removeAll xs y	

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
	| x < y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys


data BSTree a = Nil | Node a (BSTree a) (BSTree a)
                deriving (Show)


isPresentInBST :: (Ord a) => a -> BSTree a -> Bool
isPresentInBST _ Nil = False
isPresentInBST x (Node y l r) 
	| x == y = True
	| x < y = isPresentInBST x l
	| otherwise = isPresentInBST x r

testBST :: BSTree String
testBST = Node "pappa" (Node "albero" Nil Nil) (Node "zenzero" (Node "zampillo" Nil Nil) Nil)


buildBST :: [String] -> BSTree (String,Int)

buildBST xs = buildBST2 xs 1
buildBST2 [] _ = Nil
buildBST2 (x:xs) n = insertBST (x,n) (buildBST2 xs (n+1))

insertBST el Nil = Node el Nil Nil
insertBST (x,n) (Node (y,n1) l r)
	| x <= y = Node (y,n1) (insertBST (x,n) l) r
	| otherwise = Node (y,n1) l (insertBST (x,n) r)
