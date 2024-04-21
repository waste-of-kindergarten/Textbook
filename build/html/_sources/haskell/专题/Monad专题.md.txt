# Monad专题

在[函子专题](/haskell/专题/函子专题)中已经简要介绍了`Monad`类型类的定义与用法。本章将针对`Monad`展开，讲解有关`Monad`更多有关特性与用法。

## `Monad`介绍

在Haskell中，monad经常被认为是计算的抽象描述符,本质上这些描述符可以进行组合，使用较小的表达式构建更大的表达式[1](#ref1)。Monad可以作为各种数据或控制结构的标准编程接口[2](#ref2)。

> 补充： 这里的表达式有时也被称为“动作”(actions)

monad具有三个属性，这些属性使其非常高效有用：

- 模块化： monad允许计算由更简单的计算组合而合成，并将组合策略与正在执行的实际计算分开。
- 灵活性： monad使得其函数式程序相比等价的不适用单子书写的程序更具有适应性。monad将计算策略集中到一个位置，而无需分散在整个程序中。
- 隔离性：monad可以用于创建命令式计算结构，这些结构与程序主体保持安全的隔离。[2](#ref2)

`Monad`类型类定义如下：

```hs
class Applicative m => Monad m where 
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a -> m b -> m b 
    return :: a -> m a
    fail :: String -> m a
    {-# MINIMAL (>>=) #-}
```

通过定义，我们知道一个monad前提必须是一个应用函子，然而仅仅实现了某个类型的单子类型类的实例并不能保证其为真正monad。在此基础上，还应当满足以下单子律：

- 左单位元： `return a >>= k = k a`
- 右单位元： `m >>= return = m`
- 结合律：`m >>= (\x -> k x >>= h) = (m >>= k) >>= h`

## 常见的 monad

下面依次介绍几个Haskell内置的 monad，并验证其确实满足单子律。

### `Maybe` monad

`Maybe` monad定义如下：

```hs
instance Monad maybe where 
    Nothing >>= _ = Nothing
    (Just a) >>= f = f a 
```

下面我们来验证其满足单子律。

对于左单位元，要证`return a >>= k = k a`，其中`k`的类型为`a -> Maybe b`。

```hs
左边 = return a >>= k 
= Just a >>= k 
= k a = 右边
```

对于右单位元，要证`m >>= return = m`，其中`m`的类型为`Maybe a`。

```hs
当 m = Nothing 时：

左边 = m >>= return 
= Nothing >>= return 
= Nothing = m = 右边

当 m = Just a 时：

左边 = m >>= return 
= Just a >>= return 
= return a 
= Just a = m = 右边
```

对于结合律，要证`m >>= (\x -> k x >>= h) = (m >>= k) >>= h`，其中`m`的类型为`Maybe a`,`k`的类型为`a -> Maybe b`,`h`的类型为`b -> Maybe c`。

```hs
当 m = Nothing 时：

左边 = m >>= (\x -> k x >>= h)
= Nothing >>= (\x -> k x >>= h)
= Nothing 
= Nothing >>= h
= (Nothing >>= k) >>= h 
= (m >>= k) >>= h
= 右边

当 m = Just a 时：

左边 = m >>= (\x -> k x >>= h)
= Just a >>= (\x -> k x >>= h)
= k a >>= h
= (Just a >>= k) >>= h
= (m >>= k) >>= h
= 右边
```

综上，`Maybe`满足成为monad的条件。

最后，我们给出一个示例：假设我们拥有若干字符串，我们首先需要将其合并后，并计算其中非字母字符的数量。

```hs
-- code'2.hs
import Data.Char(isAlpha)

string1 = Just "My name is "

string2 = Just "L fried."
-- just joking (Alfred)
string3 = Just "The textbook seems a mess 2 readers !"

maybeMonadDemo :: Maybe Int
maybeMonadDemo = 
    string1 >>= \x -> 
        (x ++) <$> string2 >>=
            \y -> 
                (y ++) <$> string3 >>=
                    \z -> 
                        Just ( foldr (+) 0 (((\x -> if x then 0 else 1) . isAlpha) <$> z))
```

尝试在GHCi中运行查看结果：

```bash
Prelude> :load code'2.hs
[1 of 1] Compiling Main             ( code'2.hs, interpreted )
Ok, one module loaded.
Prelude> maybeMonadDemo
Just 14
```

### `List` monad

`List` monad 定义如下：

```hs
instance Monad [] where 
    xs >>= f = concatMap f xs
```

其中`concatMap`函数类型为`Foldable t => (a -> [b]) -> t a -> [b]`，当`xs`有多于一个元素时，该函数会将`f`对每个元素映射的结果进行拼接。

下面我们来验证其满足单子律。

对于左单位元，要证`return a >>= k = k a`，其中`k`的类型为`a -> [b]`。

```hs
左边 = return a >>= k 
= [a] >>= k 
= k a = 右边
```

对于右单位元，要证`m >>= return = m`，其中`m`的类型为`[a]`。

```hs
当 m = [] 时

左边 = m >>= return 
= concatMap return m 
= concatMap return []
= [] = m = 右边

当 m = (x:xs) 时，假设对于xs，右单位元成立，即`m1 >>= return = concatMap return xs = m1`成立

左边 = m >>= return 
= concatMap return m 
= concatMap return (x:xs)
= return x ++ concatMap return xs
= [x] ++ concatMap return xs 
= [x] ++ (m1 >>= return)
= [x] ++ xs -- 根据归纳假设
= x:xs = m = 右边
```

对于结合律，要证`m >>= (\x -> k x >>= h) = (m >>= k) >>= h`，其中`m`的类型为`[a]`，`k`的类型为`a -> [b]`，`h`的类型为`b -> [c]`。

```hs
当 m = [] 时

左边 = m >>= (\x -> k x >>= h)
= concatMap (\x -> k x >>= h) m
= concatMap (\x -> k x >>= h) []
= []
= [] >>= h
= ([] >>= k) >>= h
= (m >>= k) >>= h = 右边

当 m = (x:xs) 时，假设结合律对xs成立，即`xs >>= (\x -> k x >>= h) = (m1 >>= k) >>= h`成立

左边 = m >>= (\x -> k x >>= h)
= concatMap (\x -> k x >>= h) m 
= concatMap (\x -> k x >>= h) (x:xs)
= (k x >>= h) ++ (concatMap (\x -> k x >>= h) xs)
= (k x >>= h) ++ (xs >>= (\x -> k x >>= h))
= (k x >>= h) ++ ((m1 >>= k) >>= h)
= (([x] >>= k) >>= h) ++ ((m1 >>= k) >>= h)
= concatMap h ([x] >>= k) ++ concatMap h (m1 >>= k)
= concatMap h (([x] >>= k) ++ (m1 >>= k)) -- 分配律
= concatMap h ((concatMap k [x]) ++ (concatMap k xs))
= concatMap h (concatMap k ([x] ++ xs)) -- 分配律
= concatMap h (concatMap k (x:xs)) 
= concatMap h (m >>= k)
= (m >>= k) >>= h = 右边
```

这里需要证明的一点是`concatMap _`和`++`是满足分配律的，即`concatMap f l1 ++ concatMap f l2 = concatMap f (l1 ++ l2)`成立。

```hs
当 l1 = [] 时

concatMap f l1 ++ concatMap f l2 
= concatMap f [] ++ concatMap f l2 
= [] ++ concatMap f l2
= concatMap f l2
= concatMap f ([] ++ l2)
= concatMap f (l1 ++ l2)

当 l1 == (x:xs) 时，假设分配律对长度不大于xs的列表均成立

concatMap f l1 ++ concatMap f l2
= concatMap f (x:xs) ++ concatMap f l2
= concatMap f (x:xs) ++ concatMap f l2 
= concatMap f [x] ++ concatMap f xs ++ concatMap f l2 -- 归纳假设
= concatMap f [x] ++ (concatMap f xs ++ conacatMap f l2) -- 结合律 (列表满足半群结构，其中二元运算为拼接操作)
= concatMap f [x] ++ concatMap f (xs ++ l2)
= concatMap f ([x] ++ xs ++ l2) -- 归纳假设
= concatMap f ((x:xs) ++ l2) -- 结合律
= concatMap f (l1 ++ l2)
```

综上，列表满足成为monad条件。

### do-标记

### `IO` monad

-- 更多内容见系统编程一章

### `State` monad

### `Reader` monad

### `Writer` monad

<p id="ref1">[1] Merely monadic. (2021, March 16). HaskellWiki, . Retrieved 02:27, April 20, 2024 from https://wiki.haskell.org/index.php?title=Merely_monadic&oldid=64044.</p>
<p id="ref2">[2] Monad. (2022, October 22). HaskellWiki, . Retrieved 02:46, April 20, 2024 from https://wiki.haskell.org/index.php?title=Monad&oldid=65405.</p>
