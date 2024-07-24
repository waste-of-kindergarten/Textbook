# Generics 专题

***通用编程(Generic programming)*** 是一种抽象形式，允许定义操纵一大类数据类型的函数（即通用函数）[[1]](#ref1)。通用函数的实现主要依靠将数据类型`T`的值转换为同构类型`Rep T`的对应值上[[2]](#ref2)，其中`Rep`作为类型族由一组有限的类型构造器生成，通用函数只需完成对这组有限的构造器实现，即可对大量与之同构的数据类型实现操作。通常来说，我们无需手动定义Generic实例（即同构映射过程），而是通过派生将任务交给编译器。

Generic类型类（位于`GHC.Generics`）定义如下，内部包含了`Rep`类型族，以及两个函数`from`和`to`用于从数据类型到同构类型下之间的映射。

```hs
class Generic a where
  type family Rep a :: * -> *
  from :: a -> Rep a x
  to :: Rep a x -> a
  {-# MINIMAL from, to #-}
```

通用编程一般指的是 ***代数数据类型通用编程(algebric data type generic programming)*** 。代数数据类型通用编程充分利用了代数数据类型是由“零元”、“单位元”、“和类型”以及“积类型”构成这一特性，我们将这四种类型称为基本代数类型。

```hs
data V1 p  -- 零元
data U1 p = U1 -- 单位元
data (:+:) f g p = L1 (f p) | R1 (g p) -- 和
data (:*:) f g p = (f p) :*: (g p) -- 积
```



在进行同构映射的同时，我们不希望原始数据类型的信息发生丢失，因此需要额外的类型用于标记。为了记录数据类型信息，需要记录类型构造器信息，还需要记录值构造器以及选择器信息（即记录语法中的域）；另外如果有类型参数还应该记录参数信息。

首先我们先介绍 ***元信息(metadata)*** ，即数据类型信息、值构造器信息以及选择器信息。

元信息标记类型定义如下：

```hs
data D 
data C 
data S

newtype M1 i t f p = M1 {unM1 :: f p}

type D1 = M1 D 
type C1 = M1 C 
type S1 = M1 S 
```

其中`D`、`C`、`S`分别表示数据类型标记，值构造器标记以及选择器标记；`M1`类型用来表示元信息；`D1`、`C1`、`S1`为标记后的元信息标志。

除此之外，还有与元信息紧密相关的三个类型类 -- 数据类型类型类、构造器类型类和选择器类型类：

```hs
class Datatype d where 
    datatypeName :: t d (f :: k -> *) (a :: k) -> [Char]
        -- the name of the datatype
    moduleName :: t d (f :: k -> *) (a :: k) -> [Char]
        -- the fully-qualifeid name of the module where the type is declared
    packageName :: t d (f :: k -> *) (a :: k) -> [Char]
        -- the package name of the module where the type is declared
    isNewtype :: t d (f :: k -> *) (a :: k) -> Bool 
        -- marks if the datatype is actually a newtype
    {-# MINIMAL datatypeName, moduleName, packageName #-}

class Constructor c where
    conName :: t c (f :: k -> *) (a :: k) -> [Char]
        -- the name of the constructor
    conFixity :: t c (f :: k -> *) (a :: k) -> Fixity
        -- the fixity of te constructor
    conIsRecord :: t c (f :: k -> *) (a :: k) -> Bool
        -- marks if the constructor is a record
    {-# MINIMAL conName #-}

class Selector s where
    selName :: t s (f :: k -> *) (a :: k) -> [Char]
        -- the name of the selector
    selSourceUnpackedness :: t s (f :: k -> *) (a :: k) -> SourceUnpackedness
        -- the selector's unpackedness annotation (if any)
    selSourceStrictness :: t s (f :: k -> *) (a :: k) -> SourceStrictness
        -- the selector's strictness annotation (if any)
    selDecidedStrictness :: t s (f :: k -> *) (a :: k) -> DecidedStrictness
        -- the strictness that the compiler inferred for the selector
    {-# MINIMAL selName, selSourceUnpackedness, selSourceStrictness, selDecidedStrictness #-}
```

> 提示：我们后面会看到三种类型类与三种元信息标记的关系

除了元信息还有几个重要的类型标记：

```hs
data R 

newtype K1 i c p = K1 {unK1 :: c} 

type Rec0 = K1 R 
```

其中`K1`表示种类为`*`的常量、参数以及递归；`R`则作为递归标记；`Rec0`表示递归的参数。

接下来让我们创建一个数据类型并分析其在通用编程中的构造：

```hs
-- code'6.hs

{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

data Tree a = 
    Leaf a 
    | Node (Tree a) (Tree a) 
    deriving (Generic)
```

我们创建了一个二叉树数据类型，并启用`{-# LANGUAGE DeriveGeneric #-}`对二叉树进行通用派生。下面我们在GHCi中查看派生的实现是怎样的。

```bash
Prelude> :load code'6.hs
[1 of 1] Compiling Main             ( code'6.hs, interpreted )
Ok, one module loaded.
Prelude> import GHC.Generics
Prelude> :info Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
        -- Defined at code'6.hs:4:1
instance [safe] Generic (Tree a) -- Defined at code'6.hs:7:15
type instance Rep (Tree a)
  = D1
      ('MetaData "Tree" "Main" "main" 'False)
      (C1
         ('MetaCons "Leaf" 'PrefixI 'False)
         (S1
            ('MetaSel
               'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
            (Rec0 a))
       :+: C1
             ('MetaCons "Node" 'PrefixI 'False)
             (S1
                ('MetaSel
                   'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                (Rec0 (Tree a))
              :*: S1
                    ('MetaSel
                       'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (Rec0 (Tree a))))
        -- Defined at code'6.hs:7:15
```

`Rep (Tree a)`首先以`D1`开头声明了数据类型的相关信息，接着描述了该数据类型下的值构造器的构成，该类型一共由两种构造器的和组成，第一种构造器名称为`Leaf`并接受一个类型为`a`的参数；第二种为`Node`并接受两个`Tree a`作为参数。可以看到`Rep (Tree a)`与`Tree a`的构造是高度对应的。

```
D1 ...                                              ---             Tree a 相关信息
    (
            C1 .. (S1 .. (Rec0 a))                  ---             Leaf a
      :+:                                   
                C1 .. (                             ---             Node 
                        S1 .. (Rec0 (Tree a))       ---                 (Tree a)
                    :*: 
                        S1 .. (Rec0 (Tree a))       ---                 (Tree a)
                      )
    )
```


在`Rep (Tree a)`的定义中，`MetaData`、`MetaCons`、`MetaSel`分别用于真正存储数据类型信息、值构造器信息以及选择器信息，它们均为数据类型`Meta`下的构造器；另外，这三个构造器分别实现了`Datatype`、`MetaCons`以及`Selector`三个类型类的实例，形成了对应关系。

```hs
data Meta =
    MetaData Symbol Symbol Symbol Bool
    | MetaCons Symbol FixityI Bool 
    | MetaSel (Maybe Symbol) SourceUnpackedness SourceStrictness DecidedStrictness
```

## 实现通用的Show

本节我们给出如何使用通用编程实现一个类似Show的类型类(`Show'`)示例，这样只要我们能够对数据类型派生Generic，即可对这个类型的值进行展示。为了方便展示，我们在底层仍然调用了`show`函数用于展示基本的数据类型。

```hs
-- code'6.hs

-- Show' 类型类 
class Show' a where 
    show' :: a -> String 
    -- 对Generic实例默认实现
    default show' :: (Generic a,GenericShow (Rep a)) => a -> String 
    show' x = showsDefault x ""

-- 默认实现函数
showsDefault :: (Generic a, GenericShow (Rep a)) => a -> ShowS 
showsDefault x = gshow False (from x)

-- Generic 可展示类 
class GenericShow a where 
    -- ShowS :: String -> String 方便字符串进行拼接和组合
    gshow :: Bool -> a x -> ShowS 

-- 实现基础代数数据类型：零元V1
instance GenericShow V1 where 
    gshow _ _ = error "cannot show void type"

-- 实现基础代数数据类型：单位元U1
instance GenericShow U1 where 
    gshow _ U1 = id 

-- 实现基础代数数据类型：和类型:+:
instance (GenericShow a, GenericShow b) => 
    GenericShow (a :+: b) where 
    gshow b (L1 l) = gshow b l 
    gshow b (R1 r) = gshow b r 

-- 实现基础代数数据类型：积类型:*:
instance (GenericShow a, GenericShow b) => 
    GenericShow (a :*: b) where 
    gshow b (x :*: y) = gshow b x . showChar ' ' . gshow b y

-- 实现递归的参数标记：K1
instance (Show' a) => GenericShow (K1 i a) where 
    gshow _ (K1 a) = \x -> show' a ++ x 

-- 实现数据类型标记：D1
instance (GenericShow a, Datatype b) => 
    GenericShow (D1 b a) where 
    gshow b (M1 a) = gshow b a 

-- 实现值构造子标记：C1
instance (GenericShow a, Constructor g) => 
    GenericShow (C1 g a) where 
    gshow _ c@(M1 a) = showString "(" . 
                       showString (conName c) . 
                       showString " " . 
                       wrapRecord (gshow (conIsRecord c) a) . 
                       showString ")"
        where 
            -- 处理记录语法的函数
            wrapRecord :: ShowS -> ShowS 
            wrapRecord s 
                |   conIsRecord c = 
                            showString "{ " . s . showString " }"
                |   otherwise = s    

-- 实现选择器标记：S1
instance (GenericShow a, Selector g) => 
    GenericShow (S1 g a) where 
    gshow b s@(M1 a) 
        | null (selName s) = gshow b a 
        | otherwise = showString (selName s) . 
                      showString " = " . 
                      gshow b a . 
                      showChar ' '

-- 为了方便底层调用show
instance Show' Char where 
    show' = show 

instance Show' Int where 
    show' = show 

instance Show a => Show' [a] where 
    show' = show 
```

至此，我们已经实现了`Show'`类型类，接下来我们定义一些数据类型来进行检验：

```hs
-- code'6.hs

data Book = Book {name :: String, year :: Int} deriving (Generic,Show')

data Book' = Book' String Int deriving (Generic,Show')

data Nat = Zero | Succ Nat deriving (Generic,Show')

```

```bash
Prelude> show' (Book "haskell" 2024)
"(Book { name = \"haskell\"  year = 2024  })"
Prelude> show' (Book' "haskell" 2024)
"(Book' \"haskell\" 2024)"
Prelude> show' (Succ (Succ Zero))
"(Succ (Succ (Zero )))"
```

可以看到虽然我们的`Show'`不如内置的可展示类型类那样严谨，但仍然能够完整地输出有效信息。

## Generic1

在`GHC.Generics`模块中,除了`Generic`类型类外，还有`Generic1`类型类，该类型类与`Generic`类型类功能类似，不同点在于其适用于种类为`k -> *`的类型，其中`k`为种类参数表示种类多态。在实际编程中，对于两个类型类的选择往往取决于对于通用函数的设定。

有关更多Generic1的知识，读者可以翻阅[官方文档](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html#t:Generic1)进一步地了解。

-------------------------------------

<p id="ref1">[1] Generics. (2021, February 13). HaskellWiki, . Retrieved 09:09, July 23, 2024 from https://wiki.haskell.org/index.php?title=Generics&oldid=63969.
</p>
<p id="ref2">[2] GHC.Generics. (no date). Hackage,.  Retrieved 09:09, July 23, 2024 from https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html.
</p>