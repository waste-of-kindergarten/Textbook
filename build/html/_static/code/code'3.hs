import Data.IORef
import System.IO

threeStrLn :: IO Int 
threeStrLn = 
    getLine >>= 
        (\x -> getLine >>= 
            (\y -> getLine >>= (
                \z -> return $ length x + length y + length z)
            ))

threeStrLn' :: IO Int 
threeStrLn' = do 
    x <- getLine 
    y <- getLine 
    z <- getLine 
    return $ length x + length y + length z 

iorefDemo :: IO ()
iorefDemo = do 
    x <- getLine 
    aref <- newIORef x
    val <- readIORef aref 
    print val
    y <- getLine 
    writeIORef aref y 
    val <- readIORef aref 
    print val 
    modifyIORef aref ("modified :" ++ )
    val <- readIORef aref 
    print val 

fileDemo :: IO ()
fileDemo = do
        file <- openFile "test.txt" WriteMode 
        pos <- hTell file 
        print pos 
        hPutChar file 'a'
        hPutStrLn file "abc"
        hPutStr file "efg"
        pos <- hTell file 
        print pos 
        hClose file 

fileDemo' :: IO ()
fileDemo' = do 
        file <- openFile "test.txt" ReadMode 
        pos <- hTell file 
        print pos 
        c <- hGetChar file 
        print c 
        pos <- hTell file 
        print pos
        line <- hGetLine file 
        print line 
        pos <- hTell file 
        print pos
        left <- hGetContents file  
        -- hClose file 
        print left 

        
fileDemo'' :: IO ()
fileDemo'' = do 
        file <- openFile "test.txt" ReadWriteMode 
        pos <- hTell file 
        print pos 
        c <- hGetChar file 
        print c 
        pos <- hTell file 
        print pos 
        hPutStrLn file "new"
        hClose file

    