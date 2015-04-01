-- PAP a.a. 2014-2015 - Lab #02

import Screen

type Word = String

mapLen :: [Word] -> [(Word,Int)]
mapLen xs = map (\p -> (p, length p)) xs


selectedLen :: [Word] -> Int -> [(Word,Int)]
selectedLen xs w = 
	filter (\ (p,l) -> l > w) 
	       (map (\p -> (p, length p)) xs)


wordOcc :: [Word] -> Word -> Int
wordOcc xs w = foldr 
	(\p s -> if (p == w) then s + 1 else s) 0 xs 


-- data la lista di parole, calcolare la lista delle occorrenze,
-- ove ogni occorrenza è rappresentata da (Int,[String])

wordInfo :: [String] -> [(String,Int)]
wordInfo xs = map (\x -> (x, length x)) xs

addInfo :: (String,Int) -> [(Int,[String])] -> [(Int,[String])]
addInfo (s, l) [] = [(l,[s])] 
addInfo (s, l) ((l1,ws):xs) 
	| l == l1 = (l1,ws++[s]):xs
	| otherwise = (l1,ws) : (addInfo (s, l) xs) 

wordsOcc :: [String] -> [(Int,[String])]
wordsOcc xs = foldr addInfo [] (wordInfo xs)

-- esempi con traders e transactions

type TraderId = String

data Trader = Trader TraderId String
	deriving (Show)
data Transaction = Trans TraderId Int Int
	deriving (Show)

traders = [Trader "Raoul" "Cambridge", Trader "Mario" "Milan", Trader "Alan" "Cambridge", Trader "Brian" "Cesena"]
transactions = [
	Trans "Brian" 2011 300,
	Trans "Raoul" 2012 1000,
	Trans "Raoul" 2011 400,
	Trans "Mario" 2012 710,
	Trans "Mario" 2011 700,
	Trans "Alan" 2012 950]

-- Compute the set of transactions in 2011, sorted by value       

queryYear list whatYear = sort (filter (\(Trans _ year _) -> year == whatYear) list) (\(Trans _ _ value) -> value )

sort :: (Ord b) =>  [a] -> (a -> b) -> [a]
sort [] _ = []
sort (x:xs) f = 
	sort [y | y <- xs, f y <= f x] f ++ [x] ++ sort [z | z <- xs, f z > f x] f

-- elenco città distinte dove operano i trader

queryDistinctCity list = foldr (\city lst -> if (member city lst) then lst else (city:lst)) [] (map (\(Trader _ city) -> city) list)

member :: (Eq a) => a -> [a] -> Bool 
member _ [] = False
member y (x:xs) 
	| y == x = True
	| otherwise = member y xs

-- esistono trader da una certa città

queryCityTrader list city = foldr (||) False (map (\(Trader _ cty) -> cty == city) list)

-- valore massimo transazioni

queryMaxTransValue list = foldr (\(Trans _ _ v) currmax -> if (v > currmax) then v else currmax) 0 list

-- trans con val massimo

queryMaxTrans list = head reslist 
					where
						reslist = filter (\(Trans _ _ v) -> v == maxValue) list
						maxValue = foldr (\(Trans who yr v) currmax -> if (v > currmax) then v else currmax) 0 list 

-- stampa tutti i valori delle transazioni di trader che abitano a Cambridge

printAllTransOfTraderFromCity trans traders city =
	foldr (\(Trans _ _ v) act -> (putStrLn (show v)) >> act) (return ()) selTrans
	where
		tradersInCity = filter (\(Trader id cty) -> cty == city) traders
		tradersName = map (\(Trader id _) -> id) tradersInCity
		selTrans = filter (\(Trans id _ value) -> member id tradersName) trans

-- stampa valori indentati

printIndented :: (Show a) => [a] -> IO ()
printIndented xs = foldr (\s act -> putStrLn("    "++show s) >> act) (return ()) xs




