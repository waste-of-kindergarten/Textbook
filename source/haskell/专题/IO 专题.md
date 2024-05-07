# IO 专题

在[Monad专题](/haskell/专题/Monad专题)中，我们已经介绍了一些有关`IO`作为monad的性质和用法，本章我们将着重讲解使用`IO` monad进行系统编程。

## 纯度与副作用

***纯代码(purity)*** 和无 ***副作用(side effect)*** 是Haskell最基本的特性。对于一个纯函数，其结果完全由参数决定，一旦参数确定了，那么结果是唯一确定的，且不受时间或外界环境的影响。纯函数是无副作用的，与之相对的，不纯函数往往会伴随除了计算之外的行为，如程序的输入和输出等。Haskell作为程序语言，需要与现实世界产生作用，这将不可避免地带来副作用。

## `IO` monad 

Haskell将IO操作装入monad中，得到`IO` monad。一方面，我们可以在`IO` monad中执行副作用，而不会将影响传到其他纯代码中；另一方面，我们仍然可以在IO操作中进行计算，并传递计算结果。

对于一个类型为`IO a`的值，它表示该值进行若干的IO操作，并最终得到一个类型为`a`的计算结果，这个计算结果存储在monad内部，可以通过特定的方式进行传递。

回顾单子类型类的定义：

```hs
class Applicative m => Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a -> m b -> m b 
    return :: a -> m a 
    fail :: String -> m a
    {-# MINIMAL (>>=) #-}
```

`Monad`类型类的最小实现为`(>>=)`，它接受一个容器`m a`，以及一个类型为`a -> m b`的函数,并将第二个函数依次应用于第一个容器中的元素，最后生成新的容器。`return`函数将一个数值`a`映射为包含该元素的容器，对应了`Applicative`中的`pure`函数；`(>>)`类似`(>>=)`，区别是`(>>)`并没有将第一个参数传入第二个参数中；最后`fail`用于计算错误时进行报错。

使用`>>=`可以将IO操作串联起来，例如计算三个输入的字符串的长度和：

```hs
-- code'3.hs

threeStrLn :: IO Int 
threeStrLn = 
    getLine >>= 
        (\x -> getLine >>= 
            (\y -> getLine >>= (
                \z -> return $ length x + length y + length z)
            ))
```

三个`getLine`分别从键盘读取一行字符串，并分别传递给`x`，`y`和`z`,最后计算三个字符串的长度并封装到`IO` monad中。

当然上述代码也可以使用`do` 语句改写：

```hs
-- code'3.hs

threeStrLn' :: IO Int 
threeStrLn' = do 
    x <- getLine 
    y <- getLine 
    z <- getLine 
    return $ length x + length y + length z 
```

两者是完全等效的。

<center>

![](/_static/image/IOMonad.png)

</center>

## 可变数据 `Data.IORef`

在纯代码中，“变量”是不可变的，即一旦我们为变量分配了值后，就不能修改这个值。Haskell 提供了一个在`IO` monad中修改变量内存的模块`Data.IORef`，该模块提供了`IORef`容器以及相关的功能函数，使我们能够直接修改容器中的变量。

|函数名称|类型签名|功能|
|---|---|---|
| newIORef | a -> IO (IORef a) | 新建IORef |
| readIORef | IORef a -> IO a | 读取IORef中的值 | 
| writeIORef | IORef a -> a -> IO () | 向IORef写入值 |
| modifyIORef | IORef a -> (a -> a) -> IO () | 修改IORef中的值 |
| modifyIORef' | IORef a -> (a -> a) -> IO () | 严格（非惰性）修改IORef中的值 | 
| atomicModifyIORef | IORef a -> (a -> (a, b)) -> IO b | 原子地修改IORef中的值 |
| atuomicModifyIORef' | IORef a -> (a -> (a,b)) -> IO b | 原子地严格（非惰性）修改IORef中的值 |
| atomicWriteIORef | IORef a -> a -> IO () | 原子地向IORef写入值 | 
| mkWeakIORef | IORef a -> IO () -> IO (Weak (IORef a)) | 创建弱指针对象 |

上述函数中，前四个比较基础，下面给出使用示例：

```hs
-- code'3.hs

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
```

该示例首先读取一行字符串`x`，使用`newIORef`创建一个`IORef`容器`aref`，并将`x`赋值，赋值后使用`readIORef`将值读入`val`并输出；接着读取新的一行字符串`y`，并通过`writeIORef`将其写入`aref`中，写入后将值读入`val`并输出；最后，用`modifyIORef`对`aref`内的值进行修改，在其字符串前添加`"modified :"`，重新读取值到`val`并输出。

```bash
Prelude> :load code'3.hs
[1 of 1] Compiling Main             ( code'3.hs, interpreted )
Ok, one module loaded.
Prelude> iorefDemo
hello 
"hello"
hello haskell
"hello haskell"
"modified :hello haskell"
```

除了基础的四个函数操作，`atomicModifyIORef`以原子方式进行修改，该函数对于在多线程中使用`IORef`非常有用,当只有一个`IORef`时，`atomicModifyIORef`函数，可以阻止多个线程因访问其而可能产生的竞争状态；`atomicWriteIORef`以原子方式进行写入；最后`mkWeakIORef`创建一个指向`IORef`的弱指针。

> 补充：大多数现代的CPU架构都有一个内存模型，这个模型会允许线程对读取操作和写入进行重排，以使读取早于写入，例如x86/64架构，而原子方式则会强制一个内存屏障以阻止原子块中的读写操作的重排

> 提示： 有关严格求值版本的细节可参阅[Data.IORef](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-IORef.html)，有关弱指针的细节可参阅[System.Mem.Weak](https://hackage.haskell.org/package/base-4.19.1.0/docs/System-Mem-Weak.html)


## 数据读写

对于数据进行读写实际上是对于 ***句柄(handle)*** 的操作，无论对于文件或者数据流的读写，均基于句柄进行操作。句柄在Haskell中的类型为`Handle`，我们常用的标准输入输出`stdin`和`stdout`的类型均为`Handle`。数据读写相关的定义位于`Sytem.IO`。

接下来我们依托文件系统讲解有关读写的相关定义和用法。

### 读写模式

```hs
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

系统为读写模式定义了四种模式，其中`ReadMode`为只读；`WriteMode`为只写,写入内容会覆盖已经存在的内容，如果不存在则先创建；`AppendMode`为只添加,写入内容会添加到末尾，如果不存在则先创建；`ReadWriteMode`为读写，既可以读也可以写，当文件不存在时自动创建。

基于上面的模式，Haskell提供了若干打开文件的函数：

- `readFile :: FilePath -> IO String` : 读取模式打开文件

- `writeFile :: FilePath -> String -> IO ()` : 写模式打开文件

- `appendFile :: FilePath -> String -> IO ()` : 添加模式打开文件

- `openFile :: FilePath -> IOMode -> IO Handle` : （以某种模式）打开文件

上述函数中前三个函数将句柄封装起来，方便我们根据需要直接进行相关文件操作；而第四个函数`openFile`允许我们更自由操作句柄，当我们完成对句柄的操作后，需要对句柄进行关闭`hClose`。

### 句柄读操作

Haskell提供了一些句柄读操作，这里给出一些常见的函数。

- `hGetChar :: Handle -> IO Char` : 读取一个字符，访问位置向后移动一位（可能会导致isEOFError，位于`System.IO.Error`）

- `hGetLine :: Handle -> IO String` : 读取一行字符，访问位置向后移动一行（同样可能导致isEOFError）

- `hLookAhead :: Handle -> IO Char` : 向后读取一个字符，但访问位置不移动（同样可能导致isEOFError） 

- `hGetContents :: Handle -> IO String` : 读取全部内容，并自动关闭句柄

> 补充：
> 1. `hGetContents`作为惰性求值的函数，因此当我们执行这个函数时并没有读取任何字符串，这允许其读取很大的文件而不必担心内存分配问题
> 2. 似乎`hGetContents`的惰性读取字符串和关闭句柄之间是矛盾的，实际上，这个关闭状态是一种中间态，`hGetContents`仍然可以从中读取内容，虽然使用`hIsClosed`函数查看到的状态是关闭的
> 3. `hGetContents` 在句柄完全关闭的状态是无法进行读取的，因此当我们执行完`hGetContents`函数后，在使用`hClose`关闭句柄后尝试读取`hGetContents`得到的结果时，会产生异常
> 4. 因此，Haskell提供了一个严格求值版本的 `hGetContents'`

另外，针对标准输入，Haskell还提供了一些特例函数，这些函数可以认为是上述函数的部分应用函数。

- `getChar :: IO Char` :  向标准输入读取一个字符，等同于`hGetChar stdin`

- `getLine :: IO String` : 向标准输入读取一行字符串，等同于`hGetLine stdin`

- `getContents :: IO String` : 读取所有标准输入，等同于`hGetLine stdin`，严格求值版本为`getContents'`

### 句柄写操作

除了读操作，Haskell还提供了一些写操作，下面给出一些常见的函数。

- `hPutChar :: Handle -> Char -> IO ()` : 向句柄中写入一个字符
- `hPutStr :: Handle -> String -> IO ()` : 向句柄中写入一个字符串
- `hPutStrLn :: Handle -> String -> IO ()` : 向句柄中写入一个字符串，并另起一行（添加一个换行符）
-  `hPrint :: Show a => Handle -> a -> IO ()` : 向句柄中写入一个满足`Show`约束的类型的值

特别地，针对标准输出，Haskell提供了一些特例函数。

- `putChar :: Char -> IO ()` : 向标准输出写入一个字符，等同于`hPutChar stdout`

- `putStr :: String -> IO ()` : 向标准输出写入一个字符串，等同于`hPutStr stdout`

- `putStrLn :: String -> IO ()` : 向标准输出写入一个字符串，并另起一行，等同于`hPutStrLn stdout`

- `print :: Show a => a -> IO ()` : 对可展示的类型（受到`Show`约束）的值输出，等同于`\t -> hPutStrLn stdout (show t)`

### 句柄访问位置

当我们访问文件句柄时，会存在一个访问位置，从此位置可以进行读取或者写入操作。通过`hTell :: Handle -> IO Integer`函数可以获取当前句柄的绝对位置，即从初始位置到当前位置间隔的字节数目。特别地，`hIsEOF :: Handle -> IO Bool`函数用于判断是否到达了文件末尾。

我们还可以通过`hSeek`函数改变句柄访问位置，在介绍`hSeek`函数之前，我们首先了解句柄访问位置的移动参照。

```hs
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
```

其中`AbsoluteSeek`为绝对移动，即从初始位置到目标位置间隔的字节数；`RelativeSeek`为相对移动，即从当前位置移动的字节数，可以为负值（表示向前）；`SeekFromEnd`从文件末尾向前移动一定的字节数。

`hSeek`函数的类型为`Handle -> SeekMode -> Integer -> IO ()`，我们只需要提供句柄，移动参照模式以及一个偏移量即可改变句柄访问地位置。

下面提供一个综合的例子：

```hs
-- code'3.hs
  
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
```

```bash
Prelude> fileDemo
0
11
'w'
6

```

首先，程序以读写模式打开`test.txt`文件（如果没有直接创建），此时访问位置为0，从此处进行写入`"hello world"`，一共写入11个字符，因此访问位置变为11，接着移动到第六个字符的位置（访问得到此处字符为`'w'`，并保持访问位置不变），从此处写入`"new world"`之后关闭文件。重新读取文件，得到内容为覆盖后的内容`"hello new world"`。

### 缓冲区模式

一般地，从数据流中进行读取或向其中写入数据要比处理这些内容的时间长，因此系统在内存中设置 ***缓冲区(buffer)*** ，用来临时处理读写的内容，只有当缓冲区装满后才会进行刷新（即将内容真正读写到数据流中）。

在Haskell中定义了三种不同的缓冲区模式：

```hs
data BufferMode = NoBuffer | LineBuffering | BlockBuffering (Maybe Int)
```

可以看到缓冲区分为三类，`NoBuffer`表示无缓冲模式，此时字符将被逐个处理；`LineBuffering`表示行缓冲模式，字符串以行单位被处理；`BlockBuffering`表示块缓冲模式，字符串被存储在指定字节长度的缓冲区中被操作，这是Haskell默认的缓冲模式（参数设置为Nothing）。我们可以通过`hGetBuffering :: Handle -> IO BufferMode`查看句柄的缓冲区模式。

另外，我们还可以使用`hSetBuffering :: Handle -> BufferMode -> IO ()`可以改变缓冲区的模式。下面的两个示例，通过更改缓冲区模式，在缓冲区未装满的情况下，使用`hFlush :: Handle -> IO ()`函数进行手动刷新，对比刷新前后的写入情况。

```hs
-- code'3.hs

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
        getLine -- 暂停查看test.txt 内容
        hFlush file 
        getLine -- 查看刷新后的test.txt 内容
        hClose file
```

以上两个示例分别将缓冲区模式设为行缓冲模式和10字符块缓冲模式，`"abc"`不能够填满缓冲区，此时暂停查看`test.txt`文件会发现内容为空；当我们使用`hFlush`函数手动刷新后，此时查看`test.txt`文件会发现内容已经出现在文件中。

> 提示：实际上，当用户使用`hClose`函数时，`hFlush`函数将被自动调用，因此，即使我们不使用`hFlush`函数进行刷新，当我们关闭句柄后，内容仍然出现在了文件中


## 系统环境


## 文件操作

## 系统进程

## 系统时间

## 不安全的IO

---------------------------

<p id="ref1">[1] IO inside. (2024, April 22). HaskellWiki, . Retrieved 08:59, May 2, 2024 from https://wiki.haskell.org/index.php?title=IO_inside&oldid=66607.</p>

