# Generics 专题

***通用编程(Generic programming)*** 是一种抽象形式，允许定义操纵一大类数据类型的函数（即通用函数）[[1]](#ref1)。通用函数的实现主要依靠将数据类型`T`的值转换为同构类型`Rep T`的对应值上[[2]](#ref2)，其中`Rep`作为类型族由一组有限的类型构造器生成，通用函数只需完成对这组有限的构造器实现，即可对大量与之同构的数据类型实现操作。通常来说，我们无需手动定义Generic实例（即同构映射过程），而是通过派生将任务交给编译器。

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

其中`D`、`C`、`S`分别表示数据类型标记，值构造器标记以及选择器标记；`M1`类型用来表示元信息；`D1`、`C1`、`S1`为标记后的元信息。

除此之外，还有与元信息紧密相关的三个类型类：

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
import GHC.Generics (Generic)

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



-------------------------------------

<p id="ref1">[1] Generics. (2021, February 13). HaskellWiki, . Retrieved 09:09, July 23, 2024 from https://wiki.haskell.org/index.php?title=Generics&oldid=63969.
</p>
<p id="ref2">[2] GHC.Generics. (no date). Hackage,.  Retrieved 09:09, July 23, 2024 from https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html.
</p>