# Typeable 专题

***可类型化(Typeable)*** 作为一个类型类（位于`Data.Typeable`），将类型的表示与类型联系起来，使得类型得以具体化，这样我们就可以对类型进行安全的转换操作[[1]](#ref1)。由于可类型化的实现在不同版本中具有一定的差异，本章仅仅对`Typeable`类型类的实现和使用进行简单的讲解，有关更多内容读者可以查看相关文档。

## `TypeRep` 

一个比较重要的类型是`TypeRep`，该类型使用GADTs声明，如下：

```hs
data TypeRep a where 
    TrType :: TypeRep Type
    TrTyCon :: {
        trTyConFingerprint :: !Fingerprint,
        trTyCon :: !TyCon,
        trKindVars :: [SomeTypeRep],
        trTyConKind :: !(TypeRep k)
        } -> TypeRep (a :: k)
    TrApp :: forall k1 k2 (a : k1 -> k2) (b :: k1)
        {
            trAppFingerprint :: !Fingerprint,
            trAppFun :: !(TypeRep (a :: k1 -> k2)),
            trAppArg :: !(TypeRep (b :: k1)),
            trAppKind :: !(TypeRep k2)
        } -> TypeRep (a b)
    TrFun :: forall (m :: Multiplicity) (r1 :: RuntimeRep) (r2 :: RuntimeRep) 
        (a :: TYPE r1) (b :: TYPE r2).
        {
            trFunFingerprint :: !Fingerprint,
            trFunMul :: !(TypeRep a),
            trFunArg :: !(TypeRep a),
            trFunRes :: !(TypeRep b)
        } -> TypeRep (FUN m a b)
```

一共有四个构造子`TrType`、`TrTyCon`、`TrApp`、`TrFun`，其中后三个构造子中首个域均为类型的指纹，根据类型所有库名、模块名、构造器名称以及类型参数生成哈希值，可用于快速比较类型。

- `TrType`用来表示基本类型的`TypeRep`；

- `TrTyCon`用来表示类型构造器，`trTyCon`表示构造器本身，`trKindVars`表示在应用构造器时的种类参数，`trTyConKind`表示类型构造器的种类信息。例如对于`Just :: Bool -> Maybe Bool`来说，`trTyCon`将会是`Just`，而`trKindVars`会是`[Bool]`

-  `TrApp`用来表示类型构造器对类型参数的应用，`trAppFun`用来表示应用的函数，`trAppArg`表示参数，`trAppKind`表示应用结果的种类信息。例如对于`Maybe Int`来说，`trAppFun`为`Maybe`，`trAppArg`为`Int`

- `TrFun`表示函数类型，`trFunArg`和`trFunRes`分别表示函数箭头的参数与结果，例如对于函数`Int -> Bool`，有`trFunArg`为`Int`而`trFunRes`为`Bool`

`TypeRep`是可类型化中重要的数据类型，其将类型的值具体化表示出来，以便能够进行安全地对比和转换。

## `Proxy` 代理类型

在前面的章节中，我们了解到，只有种类为`*`的类型才会有值的存在，我们无法给出其他种类的类型下的值，例如`Maybe :: * -> *`就没有值与之对应。针对这类问题，我们可以采用代理类型进行代理，具体定义如下：

```hs
data Proxy (a :: k) = Proxy
```

代理类型使用了种类多态，使得其类型构造器可以接受任意种类的类型，并提供了一个值构造器`Proxy`作为其类型的值，这样我们就可以为不具有值的类型代理一个值。

```bash
Prelude> x = Proxy :: Proxy Maybe 
Prelude> x 
Proxy
Prelude> :type x
Proxy Maybe
```

## 可类型化的使用与派生

### 类型解析

Haskell中许多常见的类型已经内置了可类型化实例，我们可以使用函数查看其类型（或种类）。

对于任何可类型化类型的值来说，可以使用`typeOf`函数：

```hs
typeOf :: Typeable a => a -> TypeRep 
```

```bash
Prelude> typeOf True
Bool
Prelude> typeOf $ Just 1
Maybe Integer
Prelude> typeOf (+)
Integer -> Integer -> Integer
```

`typeOf`函数不能处理那些没有值的类型，此时我们可以使用`typeRep`并构造代理类型。

```hs
typeRep :: forall {k} proxy (a :: k). Typeable a => proxy a -> TypeRep
```

```bash
Prelude> typeRep (Proxy :: Proxy Maybe)
Maybe
Prelude> typeRep (Proxy :: Proxy Bool)
Bool
-- 当然也可以代理有值的类型
```

### 类型观察

`Data.Typeable`还提供了一些用于观察类型的函数，这些函数比较有趣。

- `funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep`

`funResultTy`将一个函数类型应用到一个值上，并应用结果包裹在`Maybe`类型中。

```bash
Prelude> plus = typeOf (+)
Prelude> plus 
Integer -> Integer -> Integer 
Prelude> x = typeOf 2
Prelude> x 
Integer 
Prelude> funResultTy plus x
Just (Integer -> Integer)
```

- `splitTyConApp :: TypeRep -> (TyCon, [TypeRep])`

`splitTyConApp`函数将一个类型构造器的应用分离。

```bash
Prelude> splitTyConApp (typeOf (Just 1))
(Maybe,[Integer])
Prelude> splitTyConApp (typeOf (Right True :: Either Bool Bool))
(Either,[Bool,Bool])
```

- `typeRepArgs :: TypeRep -> [TypeRep]`

`typeRepArgs`函数观察类型表示的参数类型，也就是`splitTyConApp`结果元组的第二个元素。

```bash
Prelude> typeRepArgs (typeOf (Right True :: Either Bool Bool))
[Bool,Bool]
```

- `typeRepTyCon :: TypeRep -> TyCon`

观察类型表示的构造函数。

```bash
Prelude> typeRepTyCon (typeOf (Right True :: Either Bool Bool))
Either
```

- `typeRepFingerprint :: TypeRep -> Fingerprint`

观察类型的指纹。

### 安全类型转换

在此之前，我们拥有函数`==`用来比较两个值是否相等，但这要求我们传入的两个值是相同类型的。对于不同类型的值，我们可以借助可类型化中提供的安全转换函数进行操作。

`cast :: (Typeable a,Typeable b) => a -> Maybe b`

```hs
-- code'4.hs

(=?) :: (Typeable a,Typeable b,Eq a,Eq b) => a -> b -> Bool
x =? y = case cast x of 
    Just x' -> x' == y 
    Nothing -> False
```

我们定义了`=?`函数用来比较两个任意类型的值是否相等，首先对`x`应用`cast`函数进行安全类型转化，这里Haskell将尝试将`x`的类型转换为`y`的类型以便满足同类型比较的要求，一旦转换成功，转换后的值`x'`被`Just`包裹并返回，接下来只需要比较`x'`和`y`即可；否则，两个值的类型不同，自然两个值就不相等。

```bash
Prelude> :load "code'4.hs"
[1 of 1] Compiling Main             ( code'4.hs, interpreted )
Ok, one module loaded.
Prelude> 1 =? 2
False
Prelude> 1 =? 1
True
Prelude> 1 =? True
False
```
### 派生

一般地，我们不会自己声明类型的可类型化实例，而采用派生的方法。当前，Haskell已经能够自动为用户创建的类型进行自动派生，而无需用户做任何操作，读者可以自行进行验证。

> 提示：关联的扩展为DeriveDataTypeable，对于较早版本无法自动派生的情况，可以考虑手动开启该扩展

----------------------------------------

<p id="ref1">[1] Data.Typeable. (no date). Hackage,. Retrieved 8:55, July 15, 2024 from https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Typeable.html.
</p>
