# Haskell 的类型系统

## Haskell 类型系统层次

Haskell 的类型系统可以认为分为了三层 -- ***值(Value)、类型(Type)、种类(Kind)***。

### 值

Haskell 中的值指的是可以参与计算或者作为计算结果的元素。由于在Haskell中函数是一等公民，因此其可以作为值一样进行传递或进行计算，与一般编程语言中讨论的值无本质区别。或者，从另一个角度来说，传统意义讨论的值在Haskell中也可以认为是一种特殊的函数。另外，运算符作为一种特殊的函数（带有缀、结合性、优先级），也被认为是值。

### 类型

Haskell 中每个值（或者可计算为值）都具有类型，这些以类型签名的形式将值与其绑定。Haskell允许类型签名中含有变量和约束，例如`id :: a -> a`的类型中含有变量`a`，`Num a => a -> a -> a`含有类型类约束`Num a`。

当值进行计算时，其中的类型也会参与类型推断，并通过类型检查对比推断结果与用户给出的类型是否一致，如果一致，则结果即为类型推断的结果；否则系统不允许这种计算，计算失败。

### 种类

种类可以认为是类型的类型，具体来讲，种类可以为类型构造器或者高阶类型操作符分配类型[[1]](#ref1)。

> 提示：虽然种类可以认为是类型的类型，但仍然应当小心其运作方式与类型的差异

种类使用`*`标记，例如对于我们常见的类型（构造器），可以查看其种类：

```bash
Prelude> :k Int
Int :: * 
Prelude> :k String
String :: *
Prelude> :k Maybe 
Maybe : * -> *
```

对于`Int`和`String`来说，其类型构造器不含任何参数，因此种类为`*`; 而`Maybe`则需要一个类型作为参数，因此种类为`*`。

值得注意的是，当且仅当种类为`*`的类型，才具有值，例如上述的`Int`和`String`类型都可以声明具有其类型的值，相对地，我们无法给出类型为`Maybe`的值，只能给出诸如`Maybe Int`或者`Maybe String`的值。

有时，处于某种目的，我们会希望在类型签名中嵌入值，此时可以将数据进行“提升”，以便值也能够作为类型参数加入到类型构造器中，一个经典的例子就是可以显示长度的列表。

```hs
-- code5.hs

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
data Nat = Zero | Succ Nat 

data ListN a n where 
    Empty :: ListN a Zero 
    Cons :: a -> ListN a n -> ListN a (Succ n)   
```

这里使用了扩展`{-# LANGUAGE DataKinds #-}`对`Nat`进行提升，使得其可以作为类型传入`ListN a n`中`n`的位置，从而表示长度。读者可能会比较困惑`ListN`数据类型的声明方式的独特性，这里为了说明示例，使用了`{-# LANGUAGE GADTs #-}`***广义代数数据类型(Generalised Algebraic Data Types)*** 扩展，其作用是针对不同的值构造子可以分配不同的类型，这在代数数据类型的声明中是不可能的。有关更多关于广义代数数据类型的使用方法，读者可以参考[专题内容](#)或者移步用户手册进行了解。

除了类型构造器外，类型类也是拥有种类的，例如：

```bash
Prelude> :k Show
* -> Constraint
Prelude> :k Functor
Functor :: (* -> *) -> Constraint
```

这里出现了一种特殊的种类`Constraint`，即受限种类。

## 类型推断

在Haskell中，类型推断围绕一个基本的演绎规则，即在函数应用时对函数以及参数做出类型上的限定。

给定函数`$f :: a \rightarrow b$`以及值`x :: a`，则必有`f x :: b`。

```math
\frac{f :: a \rightarrow b \ \ \ \ \ x :: a}{f x :: b}
```

在推断的过程中，可能需要进行相同类型的替换，这个过程称为化一。

在实际的Haskell类型推断过程中，需要注意一些复杂的情形：

- 类型类限定必须随着推断进行，不能随意丢弃

- 多态类型或受约束的类型替换为诸如`Int`的实体类型后，无法逆转

- 多态类型推演为函数类型的过程也不可逆

## 类型的秩

计算一个类型的秩定义如下：

```math
\begin{cases}
rank(t) & = 0 \\
rank(\forall t . T) & = max(1,rank(T)), \ t是T中的变量 \\
rank(T \rightarrow U) & = max(rank(T)+1,rank(U))
\end{cases}
```

其中`t`表示任意一个不含箭头以及自由变量的类型，`forall`表示一个自由变量，这个变量在`T`中以多态类型的形式存在，最后`T`和`U`表示任意的类型形式。

此前，我们讨论的均为0阶或1阶的，例如`1 :: Int`、`(&&) :: Int -> Int -> Int`均为0阶，`id :: a -> a`和`const :: a -> b -> a`均为1阶。

下面给出高阶函数的例子：

```hs
-- code5.hs
{-# LANGUAGE Rank2Types #-}

rank2demo :: (forall a . a -> a) -> b -> b 
rank2demo f = f 
```

`rank2demo`是一个二阶函数，在启用二阶扩展`{-# LANGUAGE Rank2Types #-}`后，该函数类型中可以声明局部的 ***全称量词(universal quantification)*** ，即`f :: forall a . a -> a`。这使得在应用`f`到某个值上时，`f`仍然是多态的。

对于超过二阶的函数声明，可以开启`{-# LANGUAGE RankNTypes #-}`扩展。

> 补充：一般地，将一阶函数的`forall`省略不写，实际上两者效果是等效的

## 种类多态

种类也可以具有多态的特性，***种类多态(kind polymorphism)*** 允许种类变量的存在，该变量可以取任意的种类。

使用种类多态需要开启扩展`{-# LANGUAGE PolyKinds #-}`，下面给出一个示例：

```hs
{-# LANGUAGE PolyKinds #-}

data PolyKindDemo :: forall k . k -> * 

{-
等效 
data PolyKindDemo' (t :: k) :: *
-}

type D1 = PolyKindDemo Int 

type D2 = PolyKindDemo String 
```

该示例定义一个多态种类`PolyKindDemo`，其接受任意一个种类，并最终生成一个种类为`*`的类型，`D1`和`D2`是实例化`k`后得到的两种类型。

> 补充 ： 理论上，种类多态支持任意高秩，这一点与类型多态有所不同。

## 类型族

类型族的概念来源于类型论，类型论中索引类型族是类型级别的部分函数，将函数应用于参数（类型索引）将生成一个类型[[2]](#ref2)。

在类型类一章中，我们着重讨论了***关联类型(associate types)*** ，其作为 ***类型族(type family)*** 的风格之一，可以起到替代函数依赖的作用。

考虑一个处理集合和其中元素关系的类型类示例，使用函数依赖书写如下：

```hs
-- code5.hs

{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
class Collects1 e ce | ce -> e where 
    empty1 :: ce
    insert1 :: e -> ce -> ce 
    member1 :: e -> ce -> Bool
    toList1 :: ce -> [e]

instance Eq e => Collects1 e [e] where 
    empty1 = []
    insert1 e l = e:l
    member1 e [] = False
    member1 e (x:xs) 
        | e == x = True 
        | otherwise = member1 e xs 
    toList1 l = l
```

其中`empty1`代表空集，`insert1`用来插入元素到集合中，`member1`判断元素是否在集合内，`toList1`用来将集合转化为列表。

使用关联类型风格的类型族，可以得到等价的简化代码：

```hs
-- code5.hs

{-# LANGUAGE TypeFamilies #-}
class Collects2 ce where 
    type Elem2 ce :: *
    empty2 :: ce 
    insert2 :: Elem2 ce -> ce -> ce 
    member2 :: Elem2 ce -> ce -> Bool 
    toList2 :: ce -> [Elem2 ce]

instance Eq e => Collects2 [e] where 
    type Elem2 [e] = e 
    empty2 = []
    insert2 e l = e:l
    member2 e [] = False
    member2 e (x:xs) 
        | e == x = True 
        | otherwise = member2 e xs 
    toList2 l = l
```

以上示例[[2]](#ref2)中省去了类型类的一个参数，转而使用关联类型`type Elem ce :: *`用来确定容器内的元素的类型，这实质上是一个类型级别的函数。

另一种等价的风格将类型族定义在了顶层，如下：

```hs
-- code5.hs

type family Elem3 a :: *
type instance Elem3 [e] = e

class Collects3 ce where
    empty3 :: ce 
    insert3 :: Elem3 ce -> ce -> ce 
    member3 :: Elem3 ce -> ce -> Bool 
    toList3 :: ce -> [Elem3 ce]

instance Eq e => Collects3 [e] where 
    empty3 = []
    insert3 e l = e:l
    member3 e [] = False
    member3 e (x:xs) 
        | e == x = True 
        | otherwise = member3 e xs 
    toList3 l = l
```

> 补充： 置于顶层的类型族写法也可以保留类型类双参数的写法，如下：
>
> ```hs
> -- code5.hs
> 
> class (Elem3 ce ~ e) => Collects3' e ce where
>     empty3' :: ce 
>     insert3' :: e -> ce -> ce 
>     member3' :: e -> ce -> Bool 
>     toList3' :: ce -> [e]
>
> instance Eq e => Collects3' e [e] where 
>     empty3' = []
>     insert3' e l = e:l
>     member3' e [] = False
>     member3' e (x:xs) 
>         | e == x = True 
>         | otherwise = member3' e xs 
>     toList3' l = l
> ```
> 其中`~`是一个类型函数，称为 ***相等约束(Equality constraints)*** ，它要求`Elem3 ce`的计算结果和`e`必须是相同的。这种写法与前面函数依赖的写法是完全等效的。



<p id="ref1">[1] Kind. (2017, September 28). HaskellWiki, . Retrieved 08:41, July 13, 2024 from https://wiki.haskell.org/index.php?title=Kind&oldid=62154.
</p>
<p id="ref2">[2] GHC/Type families. (2023, February 4). HaskellWiki, . Retrieved 13:55, July 13, 2024 from https://wiki.haskell.org/index.php?title=GHC/Type_families&oldid=65516.
</p>





