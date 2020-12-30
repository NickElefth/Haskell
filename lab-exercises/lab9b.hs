-- Input output

getTwo :: IO (Char, Char)
getTwo = do x <- getChar
            y <- getChar
            return (x,y)


putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = do putChar x
