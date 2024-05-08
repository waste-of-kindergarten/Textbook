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

`System.Environment`库中包含了系统环境相关的函数。这里简要介绍如何获取环境变量以及调用程序时传入的命令行参数。

### 环境变量

`getEnv`函数的类型为`String -> IO String`，该函数接受一个字符串，并将这个字符串所对应的环境包裹在IO monad中返回。例如在本机的环境中，查看`PATH`变量对应的环境。

```bash
Prelude> import System.Environment
Prelude> getEnv "PATH" >>= print
"/home/user/.vscode-server/cli/servers/Stable-b58957e67ee1e712cebf466b995adf4c5307b2bd/server/bin/remote-cli:/home/user/.opam/CP.2023.11.0~8.18~2023.11/bin:/home/user/.local/bin:/home/user/.cabal/bin:/home/user/.ghcup/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"
```

### 命令行参数

`getArgs`的类型为`IO [String]`，用于获取来自系统命令行中的参数。这里为了方便直接使用GHCi设置命令行参数，使用`getArgs`获取参数并计算参数的数目：

```bash
Prelude> :set args "1" "2" "3"
Prelude> :{
Prelude| main = do 
Prelude|   args <- getArgs
Prelude|   print $ length args
Prelude|:}
Prelude> main 
3
```

## 文件及目录操作

对于文件及目录进行操作是重要的IO操作之一，这些操作位于`System.Directory`中。

首先，`createDirectory :: FilePath -> IO ()`可以用于创建目录，与之相对的`removeDirectory :: FilePath -> IO ()`用于移除目录。然而`removeDirectory`的移除功能比较首先，其要求目录必须为空，不能有任何文件或子目录，当我们需要不加考虑删除目录时，应当使用`removeDirectoryRecursive :: FilePath -> IO ()`。

我们还可以对目录进行重命名或者移动。使用`renameDirectory :: FilePath -> FilePath -> IO ()`可以改变目录的名称；而使用`setCurrentDirectory :: FilePath -> IO ()`可以改变当前目录的路径。

除此之外，我们还需要对目录中的文件（夹）做一些操作。例如通过`getDirectoryContents :: FilePath -> IO [FilePath]`获取特定目录下的内容；`findFile :: [FilePath] -> String -> IO (Maybe FilePath)` 在指定路径下查找文件（不进行递归查找）；`renameFile :: FilePath -> FilePath -> IO ()`对文件进行重命名；`renameFile :: FilePath -> FilePath -> IO ()`复制文件到另一个目录；`removeFile :: FilePath -> IO ()`对文件进行移除。

下面给出一个示例：

```hs
-- code'3.hs

directoryDemo :: IO ()
directoryDemo = do 
        createDirectory "demo"
        writeFile "demo/test.txt" "content"
        renameFile "demo/test.txt" "demo/new.txt"
        renameDirectory "demo" "newdemo"
        getLine -- 暂停查看目录
        removeDirectoryRecursive "newdemo"
```

首先，函数创建了名为`demo`的文件夹，并向其中写入名为`test.txt`的文件，文件内容为`"content"`，接着重命名文件名为`new.txt`，重命名目录为`"newdemo"`；`getLine`函数中断程序执行，读者可以此时查看文件目录的状态，最后`removeDirectoryRecursive`函数将目录以及内部的全部内容删除。

> 有关更多的操作可以参考[System.Directory](https://hackage.haskell.org/package/directory-1.3.8.4/docs/System-Directory.html)

## 系统进程

下面介绍几个常用的系统进程相关的函数以及数据结构，读者可以自行查阅`System.Process`获取更多细节。

一个用于执行命令行命令的简单函数是`callCommand`，该函数的类型为`String -> IO ()`，当我们使用该函数执行命令时，命令的回显将直接输出到终端上。

```bash
Prelude> import System.Process
Preldue> callCommand "lsb_release -a"
No LSB modules are available.
Distributor ID: Ubuntu
Description:    Ubuntu 22.04.4 LTS
Release:        22.04
Codename:       jammy
```

有时我们需要将命令与参数分开进行分析，此时可以使用`callProess`函数，该函数的类型为`FilePath -> [String] -> IO ()`。

```bash
Prelude> callProcess "lsb_release" ["-a"]
No LSB modules are available.
Distributor ID: Ubuntu
Description:    Ubuntu 22.04.4 LTS
Release:        22.04
Codename:       jammy
```

然而有某些情况下，我们希望回显存在字符串中而不是直接输出，因此可以使用`readProcess`，该函数的类型为`FilePath -> [String] -> String -> IO String`。前两个参数与`callProcess`中参数含义对应一致，第三个参数用来表示标准输入，即执行命令后为可能存在的标准输入提供内容。

```bash
Prelude> readProcess "lsb_release" ["-a"] "abc" >>= putStrLn
No LSB modules are available.
Distributor ID: Ubuntu
Description:    Ubuntu 22.04.4 LTS
Release:        22.04
Codename:       jammy
```

下面介绍`CreateProcess`类型，该类型是调用操作系统命令基础的类型，该类型及相关定义如下：

```hs
data CmdSpec = ShellCommand String 
    | RawCommand FilePath [String]

data StdStream = Inherit 
    | UseHandle Handle
    | CreatePipe

data CreateProcess {
    cmdspec :: CmdSpec,
    cwd :: Maybe FilePath,
    env :: Maybe [(String,String)],
    std_in :: StdStream,
    std_out :: StdStream,
    std_err :: StdStream,
    close_fds :: Bool,
    create_group :: Bool,
    delegate_ctlc :: Bool,
    detach_console :: Bool,
    create_new_console :: Bool,
    new_session :: Bool,
    child_group :: Maybe GroupID,
    use_process_jobs :: Bool
} deriving (Show,Eq)
```

我们比较关心前六个参数，`cmdspec`表示命令，它要么是一条命令，此时参数与命令同为一个字符串，要么为原始命令，参数使用列表进行容纳。`cwd`表示运行命令所在的目录；`env`为执行命令添加了额外的环境变量；`std_in`、`std_out`和`std_err`表示处理标准输入输出和错误的方法，它们可以是从父继承过来、使用给定的句柄或者返回新的句柄。

一般情况下，我们只需要使用库中提供的函数即可，而无需手动配置这些参数。

`shell`函数的类型为`String -> CreateProcess`，可以用于生成带有`ShellCommand`模式的`CreateProcess`。

```bash
Prelude> shell "lsb_release -a"
CreateProcess {cmdspec = ShellCommand "lsb_release -a", cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False}
```

另一个类似的函数`proc`类型为`FilePath -> [String] -> CreateProcess`，用来生成带有`RawCommand`模式的`CreateProcess`。

```bash
Prelude> proc "lsb_release" ["-a"]
CreateProcess {cmdspec = RawCommand "lsb_release" ["-a"], cwd = Nothing, env = Nothing, std_in = Inherit, std_out = Inherit, std_err = Inherit, close_fds = False, create_group = False, delegate_ctlc = False, detach_console = False, create_new_console = False, new_session = False, child_group = Nothing, child_user = Nothing, use_process_jobs = False}
```

一旦我们创建了`CreateProcess`类型的值，就可以使用`createProcess`函数执行。`createProcess`的类型为`CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle,ProcessHandle)`，返回的IO monad中包含了三个句柄，分别为输入，标准输出和错误输出，第四个`ProcessHandle`表示进程句柄，可用于后续程序等待其终止。

```bash
Prelude> createProcess $ proc "lsb_release" ["-a"]
```

当然，根据需要我们也可以手动创建`CreateProcess`，例如指定输入输出的句柄，使其重定向到合适的数据流（如文件或者网络）。这里不再过多赘述，感兴趣的读者可以自行尝试。

有时，一个函数执行会消耗一定的时间，一般情况下我们可以在这段时间里处理其他的计算，然而，某些特殊的情况可能要求我们等待进程结束，此时可以使用`waitForProcess :: ProcessHandle -> IO ()`函数。

```bash
Prelude> createProcess (proc "sleep" ["3"]) >>= \(_,_,_,t) -> waitForProcess t >> print "wait for the process to be done"
"wait for the process to be done"
Prelude> createProcess (proc "sleep" ["3"]) >> print "without waiting for the process to be done"
"without waiting for the process to be done"
```

我们还可以使用`getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)`获取进程执行的退出码。

```bash
Prelude> createProcess (proc "sleep" ["3"]) >>= \(_,_,_,t) -> waitForProcess t >> getProcessExitCode t
Just ExitSuccess
```

> 提示: `getProcessExitCode`需要与`waitForProcess`联合使用，否则返回的永远都是`Nothing`

有时我们还希望直接结束某些耗时很长的进程：

```bash
Prelude> createProcess (proc "sleep" ["100"]) >>= \(_,_,_,t) -> terminateProcess t >> waitForProcess t >> getProcessExitCode t
Just (ExitFailure (-15))
```

使用`terminateProcess :: ProcessHandle -> IO ()`可以终止程序，上述示例中，可以看到进程被强制终止后，获得的退出码是异常退出。

## 不安全的IO

---------------------------

<p id="ref1">[1] IO inside. (2024, April 22). HaskellWiki, . Retrieved 08:59, May 2, 2024 from https://wiki.haskell.org/index.php?title=IO_inside&oldid=66607.</p>

