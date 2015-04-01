module Screen (Pos,cls,goto,beep,writeAt) where

type Pos = (Int,Int)

-- clear the text screen
cls :: IO()
cls = putStr "\ESC[2J"

-- make a beep
beep :: IO()
beep = putStr "\BEL"

-- move the cursor at the new pos (x,y)
goto :: Pos -> IO()
goto (x,y) = 
	putStr("\ESC["++show y++";"++show x++"H")

-- write at the cursor
writeAt :: Pos -> String -> IO()
writeAt p s = goto p >> putStr s
				



