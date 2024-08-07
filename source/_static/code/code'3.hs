import Data.IORef
import System.IO
import System.Directory
import System.IO.Unsafe 


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
        file <- openFile "test.txt" ReadWriteMode 
        pos <- hTell file 
        print pos
        hPutStr file "hello world"
        pos <- hTell file 
        print pos 
        hSeek file AbsoluteSeek 6
        c <- hLookAhead file 
        print c
        pos <- hTell file 
        print pos 
        hPutStr file "new world"
        hClose file
        x <- readFile "test.txt"
        print x 

    
setLineBufferingDemo :: IO () 
setLineBufferingDemo = do 
        file <- openFile "test.txt" WriteMode 
        hSetBuffering file LineBuffering 
        hPutStr file "abc"
        getLine -- 暂停查看test.txt 内容
        hFlush file 
        getLine -- 查看刷新后的test.txt 内容
        hClose file 

setBlockBufferingDemo :: IO () 
setBlockBufferingDemo = do 
        file <- openFile "test.txt" WriteMode 
        hSetBuffering file (BlockBuffering (Just 10))
        hPutStr file "abc"
        getLine 
        hFlush file 
        getLine 
        hClose file 

directoryDemo :: IO ()
directoryDemo = do 
        createDirectory "demo"
        writeFile "demo/test.txt" "content"
        renameFile "demo/test.txt" "demo/new.txt"
        renameDirectory "demo" "newdemo"
        getLine -- 暂停查看目录
        removeDirectoryRecursive "newdemo"

val :: IORef Int 
val = unsafePerformIO $ newIORef 0  

val' :: IORef Int 
val' = unsafePerformIO $ newIORef 0

unsafeDemo1 :: IO ()
unsafeDemo1 = do 
    x <- unsafeInterleaveIO $ readIORef val 
    y <- unsafeInterleaveIO $ writeIORef val 1 >> readIORef val 
    print x 
    print y 

unsafeDemo2 :: IO ()
unsafeDemo2 = do 
    x <- unsafeInterleaveIO $ readIORef val'
    y <- unsafeInterleaveIO $ writeIORef val' 1 >> readIORef val' 
    print y 
    print x 