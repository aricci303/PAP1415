
-- Corso PAP 2014-2015 - ISI-LM - UNIBO
-- Sorgenti soluzioni Assignment #2

import Screen

type P2d = (Int, Int)
type V2d = (Int, Int)

data Shape = Line P2d P2d | Tri P2d P2d P2d | Rect P2d P2d | Circle P2d Int | Combo [Shape]
	deriving (Show)

class CShape s where
	perim :: s -> Float
	move :: s -> V2d -> s

instance CShape Shape where
	perim (Line p1 p2) = dist p1 p2
	perim (Rect (x0,y0) (x1,y1)) = (abs(fromIntegral (x1-x0))+abs(fromIntegral(y1-y0)))*2
	perim (Tri p0 p1 p2) = dist p1 p0 + dist p2 p1 + dist p2 p0
	perim (Circle _ r) = pi * fromIntegral r * 2 
	perim (Combo xs) = foldr (+) 0 (map (\x -> perim x) xs)
	move (Line p1 p2) dv = Line (addV2d p1 dv) (addV2d p2 dv)
	move (Rect p1 p2) dv = Rect (addV2d p1 dv) (addV2d p2 dv)
	move (Tri p1 p2 p3) dv = Tri (addV2d p1 dv)(addV2d p2 dv)(addV2d p3 dv)
	move (Circle c r) dv = Circle (addV2d c dv) r
	move (Combo xs) dv = Combo (map (\x -> move x dv) xs)

dist :: P2d -> P2d -> Float
dist (x0,y0) (x1,y1) = sqrt((fromIntegral x1-fromIntegral x0)^2+(fromIntegral y1-fromIntegral y0)^2)

addV2d :: P2d -> V2d -> P2d
addV2d (x,y) (dx,dy) = (x+dx,y+dy)

--

moveShapes :: [Shape] -> V2d -> [Shape]
moveShapes xs dv = map (\x -> move x dv) xs

inBBox :: [Shape] -> P2d -> P2d -> [Shape]
inBBox xs p0 p1 = filter (\s -> isInBBox s p0 p1) xs

isInBBox :: Shape -> P2d -> P2d -> Bool
isInBBox (Line v0 v1) p0 p1 = (isInBB v0 p0 p1) && (isInBB v1 p0 p1)
isInBBox (Tri v0 v1 v2) p0 p1 = (isInBB v0 p0 p1) && (isInBB v1 p0 p1) && (isInBB v2 p0 p1)
isInBBox (Rect v0 v1) p0 p1 = (isInBB v0 p0 p1) && (isInBB v1 p0 p1) 
isInBBox (Circle (x0,y0) r) p0 p1 = 
		(isInBB (x0-r,y0) p0 p1) && (isInBB (x0+r,y0) p0 p1) &&
		(isInBB (x0,y0-r) p0 p1) && (isInBB (x0,y0+r) p0 p1) 
isInBBox (Combo xs) p0 p1 = foldr (&&) True (map (\x -> isInBBox x p0 p1) xs)

isInBB :: P2d -> P2d -> P2d -> Bool
isInBB (x,y) (x0,y0) (x1,y1) = (x >= x0) && (x <= x1) && (y >= y0) && (y <= y1)

maxArea :: [Shape] -> Shape
maxArea xs = fst (foldr (\ (s1,a1) (s2,a2) -> if (a1 > a2) then (s1,a1) else (s2,a2)) (Combo [],-1) (map (\s -> (s,area s)) xs)) 	

area :: Shape -> Float
area (Line _ _) = 0
area (Rect (x0,y0) (x1,y1)) = abs (fromIntegral x1 - fromIntegral x0) * abs (fromIntegral y1 - fromIntegral y0)
area (Circle _ r) = pi * fromIntegral (r*r)
area (Combo xs) = foldr (+) 0 (map (\s -> area s) xs)
area (Tri (x0,y0) (x1,y1) (x2,y2)) = 
	abs ((yb - ya)*(xc - xb) + (yb-yc)*(xb-xa))*0.5
	where 	xa = fromIntegral x0;
			ya = fromIntegral y0;
			xb = fromIntegral x1;
			yb = fromIntegral y1;
			xc = fromIntegral x2;
			yc = fromIntegral y2

data BSTree a = Nil | Node a (BSTree a) (BSTree a)
                deriving (Show)

insertBST :: a -> BSTree a -> (a -> a -> Bool) -> BSTree a
insertBST el Nil _ = Node el Nil Nil
insertBST x (Node y l r) f
	| f x y = Node y (insertBST x l f) r
	| otherwise = Node y l (insertBST x r f)

makeShapeTree :: [Shape] -> BSTree Shape
makeShapeTree xs = foldr (\s t -> insertBST s t (\s1 s2 -> minPointX s1 < minPointX s2)) Nil xs

minPointX :: Shape -> Int
minPointX (Line (x0,y0) (x1,y1)) = min x0 x1
minPointX (Rect (x0,y0) (x1,y1)) = min x0 x1
minPointX (Circle (xc,yc) r) = xc - r
minPointX (Tri (x0,y0) (x1,y1) (x2,y2)) = foldr (\a b -> if a < b then a else b) x0 [x1,x2]
minPointX (Combo (x:xs)) = foldr (\a b -> if a < b then a else b) (minPointX x) (map (\s -> minPointX s) xs)

--

class (CShape s) => Drawable s where
	draw :: s -> IO()

instance Drawable Shape where
	draw (Rect (x0,y0) (x1,y1)) = do
		draw (Line (x0,y0) (x1,y0))
		draw (Line (x1,y0) (x1,y1))
		draw (Line (x0,y0) (x0,y1))
		draw (Line (x0,y1) (x1,y1))
	draw (Line (x0,y0) (x1,y1))
		| x1-x0 > y1-y0 = drawLnDX x0 (fromIntegral y0) x1 deltayx
		| otherwise = drawLnDY (fromIntegral x0) y0 y1 deltaxy
									where deltayx = (fromIntegral (y1-y0)/fromIntegral (x1-x0));
										  deltaxy = (fromIntegral (x1-x0)/fromIntegral (y1-y0))
	draw (Tri p0 p1 p2) = do
		draw (Line p0 p1)
		draw (Line p1 p2)
		draw (Line p0 p2)
	draw (Circle (xc,yc) r) = drawCr (xc,yc) (fromIntegral r)xw 0

drawLnDX :: Int -> Float -> Int -> Float -> IO ()
drawLnDX x y xtarget delta 
	| x == xtarget = writeAt (xtarget, round y) "*"
	| otherwise = do
		writeAt (x, round y) "*"
		drawLnDX (x+1) (y+delta) xtarget delta

drawLnDY :: Float -> Int -> Int -> Float -> IO ()
drawLnDY x y ytarget delta 
	| y == ytarget = writeAt (round x, ytarget) "*"
	| otherwise = do
		writeAt (round x, y) "*"
		drawLnDY (x+delta) (y+1) ytarget delta

drawCr :: P2d -> Float -> Float -> IO()
drawCr (xc,yc) r rad 
	| rad >= 2*pi = return ()
	| otherwise = do
		writeAt (px,py) "*"
		drawCr (xc,yc) r (rad+0.1)
			where 	px = round( fromIntegral xc + (cos rad)*r);
					py = round( fromIntegral xc - (sin rad)*r)

--

drawAll :: [Shape] -> IO ()
drawAll xs = foldr (>>) (return ()) (map draw xs)

--

testShapes :: [Shape]
testShapes = [ Rect (3,5) (10,10), Line (1,1) (8,8), Circle (4,4) 2, Tri (1,3) (3,1) (3,3)]


