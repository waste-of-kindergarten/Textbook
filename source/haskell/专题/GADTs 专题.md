# GADTs 专题

***广义代数数据类型(Generalised Algebraic Data Types, GADTs)*** 扩展了普通代数数据类型，允许构造函数拥有更丰富的返回类型[[1]](#ref1)。

如果读者来自[Haskell 的类型系统](/haskell/Haskell%20的类型系统)一章，那么应该已经看到使用GADTs定义的带长度的列表数据类型`ListN`，让我们简单回顾一下。

```hs
-- code'5.hs

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

data Nat = Zero | Succ Nat 

data ListN a (n :: Nat) where 
    Empty :: ListN a Zero 
    Cons :: a -> ListN a n -> ListN a (Succ n)   
```

首先我们定义了自然数类型`Nat`，该类型由两个构造子组成，其中`Zero`表示零，`Succ Nat`表示任意自然数的后继。

接着我们需要定义一个带有长度信息的列表，因此类型构造子需要两个参数，列表元素的类型`a`以及列表的长度`n`。对于类型构造子，其参数必须是一个类型，为了将自然数作为参数传入`n`的位置，使用`{-# LANGUAGE DataKinds #-}`签名将自然数进行“提升”。

GADTs定义数据类型时以`data`关键字开头，并使用`where`关键字引出构造子的定义，每个构造子都伴随一个类型签名，用来描述构造子的构造方式。在示例中，第一个构造子`Empty`代表空列表，不需要接受任何参数，最终得到的类型为`ListN a Zero`；第二个构造子`Cons`对应将一个元素`a`附加在另一个列表`ListN a n`上，得到新的列表，新列表的长度增加1，因此最终得到的类型为`ListN a (Succ n)`。

可以看到，对于不同长度的列表，即使内部元素相同，其返回的类型也是不同的。这是无法通过普通代数数据类型做到的。

> 注意： 在示例中，构造子的类型签名中出现的参数`a`和`n`与定义首行的`ListN a (n :: Nat)`中的`a`和`n`没有关联，而是独立的代表多态的变量。

使用GADTs是比较灵活的，具体表现如下：

- 可以在构造子签名中使用上下文约束，即允许`[Constructor] :: [Constraint] => [Signature]`样式的声明

```hs
-- code'5.hs

-- 对列表元素进行数字类约束
data NumListN a (n :: Nat) where 
    NumEmpty :: Num a => NumListN a Zero 
    NumCons :: Num a => a -> NumListN a n -> NumListN a (Succ n)
```

- 可以在构造子的签名中使用记录语法

```hs
-- code'5.hs

-- 使用记录语法改写的列表
data ListN' a (n :: Nat) where 
    Empty' :: ListN' a Zero
    Cons' :: {head' :: a , tail' :: ListN' a n} -> ListN' a (Succ n) 
```

> 注意： 当我们使用记录语法时，且不同的构造子中的域名称有重复时，应当保证域的类型是相同的

- 可以在构造子签名中使用存在量化，即返回类型中不含有受约束的变量

```hs
-- code'5.hs

-- 由NumListN改写的异构列表，使得列表中的元素可以为满足Num约束的任何类型
data HeteNumListN (n :: Nat) where 
    HeteNumEmpty :: HeteNumListN Zero 
    HeteNumCons :: Num a => a -> HeteNumListN n -> HeteNumListN (Succ n)
```

- 使用普通代数数据类型的数据类型完全可以用GADTs声明

```hs
-- code'5.hs

-- 改写Nat类型
data Nat' where 
    Zero' :: Nat' 
    Succ' :: Nat' -> Nat' 
```

- 我们无法对广义代数数据类型使用派生（等效普通代数数据类型的除外），但我们仍然可以使用孤立派生`{-# LANGUAGE StandaloneDeriving #-}`声明

```hs
-- code'5.hs

-- 使用孤立派生实现ListN的Show类型类实例
deriving instance Show a => Show (ListN a n)
```





---------------------------------------

<p id="ref1">[1] 6.4.8. Generalised Algebraic Data Types (GADTs) — Glasgow Haskell Compiler 9.10.1 User's Guide. (2023). Haskell,. Retrieved 20:58, July 14, 2024 from https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/gadt.html.
</p>