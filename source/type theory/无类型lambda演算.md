# 无类型lambda演算(Untyped lambda calculus)

## lambda-项(lambda-terms)

在lambda计算中,各种表达式被称为` $\lambda-terms$ ` (`$\lambda$` -项).设 ` $\Lambda$ ` 为所有 ` $\lambda$ ` -项的集合,并假设存在一个无穷集合` $V$ `,其中的元素称为变量(variables).

` $
\begin{aligned}
& 定义 \ \ \ (\lambda-term) \\
& (1) (变量) \ 如果 \ u \in V , 那么 \ u \in \Lambda.  \\
& (2) (应用) \ 如果 \ M 和 N \in \Lambda , 那么 \ (M N) \in \Lambda. \\
& (3) (抽象) \ 如果 \ u \in V 且 M \in \Lambda , 那么 \ (\lambda u \ . \ M) \in \Lambda.
\end{aligned}
$ `

以上定义使用抽象语法(abstract syntax)也可以写成 ` $\Lambda = V | (\Lambda \Lambda) | (\lambda V. \Lambda) $ `.

由` $\lambda-项$ ` 的定义自然引入了子项(subterms)的概念.对于任意一个` $\lambda$ ` -项,其全部子项构成了一个多重集(multiset).

` $
\begin{aligned}
&定义 \ \ \ (Multiset \  of \ subterms; Sub) \\
&(1) (基本) \ 对任意的x \in V , 有Sub(x) = \{x\}. \\
&(2) (应用) \ Sub(M N) = Sub(M) \cup Sub(N) \cup \{M N\}. \\
&(3) (抽象) \ Sub(\lambda \  x \ . M ) = Sub(M) \cup \{\lambda \ x \ . M\}
\end{aligned}
$ `

如果` $L \in Sub(M)$ `,则称 ` $L$ ` 是 ` $M$ ` 的一个子项.根据子项定义,可以得出以下性质:

` $
\begin{aligned}
& 引理 \\
& (1) (自反性) \ 对于任意的\lambda-项 \ M,都有 M \in Sub(M). \\
& (2) (传递性) \ 如果 L \in Sub(M) 且 M \in Sub(N),则 L \in Sub(N). 
\end{aligned}
$ `

类似子集的概念,如果一个 ` $\lambda$ `-项的子项不是它本身,那么称为真子项(proper subterm).

` $
\begin{aligned}
定义 \ \ \ (proper \ subterm)  \ \  如果 L 是 M 的一个子项,但 L \not \equiv M,则称L是M的真子项. 
\end{aligned}
$ `

>规定:
>
>- 表达式最外层括号可以省略.
>- 应用操作是**左结合**的,因此 ` $ (M N) L $ ` 可以化为 ` $M N L$ ` .
>- 抽象操作是**右结合**的,因此 ` $\lambda x \ . \ (\lambda y  \ . \ M)$ ` 可以化为 ` $\lambda xy \ . \ M$ `. 
>- 应用的优先级高于抽象,因此` $\lambda x \ . \ M N$ `表示的含义是 ` $\lambda x \ . \ (M N)$ `

## 自由变量,约束变量和绑定变量(Free variables,bound variables and binding variables)

在` $\lambda$ `-项中,变量的情形可以分为三类, 自由情形(free occurrences)、约束情形(bound occurrences)、绑定情形(binding occurrences).

通过` $\lambda$ `-项的构造来说明这三种变量情形,

- 单独的变量是自由的(如 ` $x$ `),因此属于自由变量.
- 通过抽象操作,可以将表达式中的一个自由变量变成约束变量( 如 ` $x$ ` 在 ` $M$ ` 中是自由的 , 但 ` $\lambda x \ . \ M$ ` 使 M中的 ` $x$ `变为受约束的).
- 在第二点中,紧跟随 ` $\lambda$ ` 的变量被称为绑定变量(对 ` $M$ `中的 ` $x$ ` 进行抽象绑定了$M$中所有自由的 ` $x$ `).

在以上三种情形中,自由变量的情形比较重要,约定 ` $FV(L)$ ` 表示` $L$ `中的自由变量组成的集合.

` $
\begin{aligned}
& 定义 \ \ \ (FV , \ the \ set \ of \ free \ variables \ of \ a \ \lambda-term) \\
& (1) (变量) \ FV(x) = \{x\}. \\
& (2) (应用) \ FV(MN) = FV(M) \cup FV(N). \\
& (3) (抽象) \ FV(\lambda x \ . \ M) = FV(M) \setminus \{x\}.
\end{aligned} 
$ `

根据自由变量的数目,可以划分出` $\lambda$ `-项中一类特殊的子集,该子集中的元素被称为结合子(combinator).

` $
\begin{aligned}
定义 \ \ \ (Closed \ \lambda-term; combinator ; \Lambda^0) \ \  对于给定的 \lambda-项 M, \\ 如果FV(M) = \emptyset , 则称M为闭的.一个封闭的\lambda-项也被称为结合子.
\end{aligned}
$ `

## Alpha 转换 (Alpha conversion)

对于不同的 ` $\lambda$ ` -项,其表达的语法可能是相同的,例如` $\lambda x \ . \ x \equiv \lambda u \ . \ u$ `.为了描述这种关系,我们定义了` $\alpha$ ` -转换(alpha-conversion)或者` $alpha$ ` -等价(alpha-equivalence).

` $
\begin{aligned}
&定义 \ \ \ (Renaming ; M^{x \rightarrow y} ; =_\alpha) \\
&对于一个表达式M,将M中的所有自由变量 x 替换成 y \ (其中\ y \not \in FV(M) 且 y 在 M 中不是绑定变量), \\ & 设结果为M^{x \rightarrow y}, 则有\lambda x \ . \ M =_\alpha \lambda y \ . \ M^{x \rightarrow y}.
\end{aligned}
$ `

我们称这种情形为` $\lambda x \ . \ M$ ` 被重命名为 ` $\lambda y \ . \ M^{x \rightarrow y}$ `.

根据基本的定义,可将其更一般化,定义如下.

` $
\begin{aligned}
&定义 \ \ \ (\alpha-conversion or \alpha-equivalence, =_\alpha) \\
&(1) (重命名) \ \lambda x \ . \ M =_\alpha \lambda y \ . \ M^{x \rightarrow y},条件与上面的定义相同 \\
&(2) (兼容性) \ 如果 M =_\alpha N, 那么 M L =_\alpha N L, L M =_\alpha L N 且,对任意的z, \lambda z \ . \ M =_\alpha \lambda z \ . \ N, \\
&(3) (自反性) \ M =_\alpha M, \\
&(4) (对称性) \ 如果 M =_\alpha N , 那么 N =_\alpha M, \\
&(5) (传递性) \ 如果 L =_\alpha M 且 M =_\alpha N 成立, 那么 L =_\alpha N 成立.
\end{aligned}
$ `

> 总之,这种变换不允许表达式中其他变量的状态发生变化,比如从自由变量变成了约束变量.

因此,重命名被视为 ` $\alpha$ `-等价的一种情形.由于自反性,对称性,传递性均成立,因此` $\alpha$ `-等价是一种等价关系,通过这种等价关系可以将表达式分为许多等价类,每个等价类之间的元素均是` $\alpha$ `-等价的,也被称为是` $\alpha$ `-可转换的(` $\alpha-convertible$ `)或者说其中一个元素是另一个元素的` $\alpha$ `-变体(` $\alpha-variant$ `).

` $
\begin{aligned}
&定义 \ \ \ (\alpha-convertible;\alpha-equivalent;\alpha-variant) \\
& 如果 M =_\alpha N,那么M和N称为 \alpha-可转换的 或者 \alpha-等价的. M被称为N的一个\alpha-变体(反之亦然).
\end{aligned}
$ `

## 替换(Substitution)

替换顾名思义就是将表达式中的变量替换成其他的变量和表达式.

` $
\begin{aligned}
&定义 \ \ \ (Substitution) \\
& (1a) \ x [x := N] \equiv N, \\
& (1b) \ 设 x \not \equiv y ,  有y[x := N] \equiv y, \\
& (2) \ (PQ)[x := N] \equiv (P[x := N]) (Q [x := N]), \\
&(3) \ 如果 \lambda z \ . \ P^{y \rightarrow z} 是\lambda y \ . \ P 的\alpha-变体,且z \not \in FV(N), 则(\lambda y \ . \ P)[x := N] \equiv \lambda z \ . \ (P ^{y \rightarrow z}[x := N])
\end{aligned}
$ `

> 当` $y \not \in FV(N)$ `时,有` $(\lambda y \ . \ P)[x := N] \equiv \lambda y \ . \ (P^{y -> y}[x := N]) \equiv \lambda y \ . \ (P[x := N])$ `.

替换的顺序在交换后满足某种等价关系.

` $
\begin{aligned}
& 引理 \ \ \ 设 x \not \equiv y 且 x \not \in FV(L). 则有  M [x := N] [y := L] \equiv M[y := L] [x:=N[y := L]]. 
\end{aligned}
$ `

## Lambda-项 模 alpha-等价 (lambda-terms modulo alpha-equivalence)

` $\alpha$ `-等价关系不随着基本操作而发生改变.

` $
\begin{aligned}
& 引理 \ \ \ 设 M_1 =_\alpha N_1 且 M_2 =_\alpha N_2. 则有 \\
& (1) \ M_1 N_1 =_\alpha M_2 N_2, \\
& (2) \ \lambda x \ . \ M_1 =_\alpha \lambda x \ . \ N1 ,  \\
& (3) \ M_1[x := N_1] =_\alpha M_2[x := N_2].
\end{aligned}
$ `

规定` $\alpha$ `-等价的项是语法等同的,即若` $M =_\alpha N$ `有 ` $M \equiv N$ `.

在进行操作时,经常会遇到绑定变量重名的情况, 这可能会造成一定的困惑,Barendregt 规约(Barendregt convention)给出了一种方式使表达式更容易阅读.

> (Barendregt convention ) 我们为一个` $\lambda$ `-项中绑定变量选择名字,使它们互不相同且不同于任何出现过的自由变量.
> 示例: ` $(\lambda xy \ . \ xz) (\lambda xz \ . \ z) \rightarrow (\lambda xy \ . \ xz) (\lambda uv \ . \ v)$ `

## Beta-归约 (Beta reduction)

对于一个表达式,提供了一种化简方法,称为` $\beta$ `-归约(Beta reductoin).

` $
\begin{aligned}
& 定义 \ \ \ (One-step \ \beta-reduction, \rightarrow_\beta) \\
& (1) (基本) \ (\lambda x \ . \ M) N \rightarrow_\beta M[x := N], \\
& (2) (兼容性) \ 如果 M \rightarrow_\beta N , 则 M L \rightarrow_\beta N L , L M \rightarrow_\beta L N 且 \lambda x \ . \ M \rightarrow_\beta \lambda x \ . \ N .
\end{aligned}
$ `

一个单步` $\beta$ `-归约可以表示如下:
` $
\dots ((\lambda x \ . \ M) N) \dots \ \ \rightarrow_\beta \ \ \dots (M[x := N]) \dots
$ `
上式中左侧"` $(\lambda x \ . \ M ) N$ `"子项称为可约表达式(redex , reducible expression);右侧"` $M[x := N]$ `"子项称为矛盾统(contractum).

对于某些表达式,使用单步` $\beta$ `-归约可以导致不同的结果,但多次使用单步$\beta$-归约并不一定会得到相同的结果"公约式".

例如
` $
(\lambda x \ . \ x x)(\lambda x \ . \ x x) \rightarrow_\beta (\lambda x \ . \ x x) (\lambda x \ . \ x x)
$ `
在归约过程中,往往需要不止一次单步` $\beta$ `-归约,由此引出了更一般的` $\beta$ `-归约(记为` $\twoheadrightarrow_\beta$ `).

` $
\begin{aligned}
& 定义 \ \ \ (\beta-reduction \ (zero-or-more-step),\twoheadrightarrow_\beta)  \\
& 如果存在一个n \ge 0 \ 且 存在若干项\ M_0 \ 到\ M_n \ 使得\ M_0 \equiv M, M_n \equiv N  , M \twoheadrightarrow_\beta N \\  & 且对任意的 0 \le i \lt n ,  M_i \rightarrow_\beta  M_{i+1} , 则有 
\ M \twoheadrightarrow_\beta N.
\end{aligned}
$ `

下面给出 ` $\beta$ `-归约的性质.

` $
\begin{aligned}
& 引理 \\
& (1) \ \twoheadrightarrow_\beta 是 \rightarrow_\beta的延伸,如果M \rightarrow_\beta N ,则 M \twoheadrightarrow_\beta N \\
& (2) \ \twoheadrightarrow_\beta 是自反的和传递的, \\
& \  (自反性) \ 对于任意的M : M \twoheadrightarrow_\beta M , \\
& \ (传递性) \ 对于任意的L , M 和 N : 如果 L \twoheadrightarrow_\beta M 且 M \twoheadrightarrow_\beta N , 则 L \twoheadrightarrow_\beta N. 
\end{aligned}
$ `

通过对` $\beta$ `-归约进行扩展得到` $\beta$ `-转换,记为` $=_\beta$ `.

` $
\begin{aligned}
& 定义 \ \ \ (\beta-conversion, \beta-equality ; =_\beta )\\
& 若存在 n \ge 0 且 存在若干项 M_0 到 M_n 使得 M_0 \equiv M , M_n \equiv N 以及 对于任意的0 \le i \lt n , 以下关系之一成立: \\
&M_i \rightarrow_\beta M_{i+1} 或者 M_{i+1} \rightarrow_\beta M_i, \\ 
& 则称 M =_\beta N .
\end{aligned}
$ `

以上关系又可读作"M 和 N 是 ` $\beta$ `-可转换的 或 ` $\beta$ `-相等".

` $\beta$ `-转换有如下性质:

` $
\begin{aligned}
& 引理 \\
& (1) =_\beta 是  \twoheadrightarrow_\beta 在双向上的延伸,即 如果 M \twoheadrightarrow_\beta N 或 N \twoheadrightarrow_\beta M , 则 M =_\beta N. \\
& (2) \ =_\beta 是一个等价关系,因此有自反性,对称性和传递性 \\
& \ (自反性) \ 对于任意的M : M =_\beta M, \\
& \ (对称性) \ 对于任意的M和N : 如果 M =_\beta N , 则 N =_\beta M, \\
& \ (传递性) \ 对于所有的L , M 及 N : 如果 L =_\beta M 且 M =_\beta N , 则 L =_\beta N.
\end{aligned}
$ `

## 范式与合流性(Normal forms and confluence)

` $\beta$ `-化归过程相当于对函数带入值得过程,对于一般的数值计算往往都会得到一个数字结果,但对于` $\lambda$ `-项来说是不一定的,由此我们引出 ` $\beta$ `-范式的概念.

` $
\begin{aligned}
& 定义 \ \ \ (\beta-normal \ form; \beta-nf; \beta-normalising) \\
& (1) \ 若M不含有任何的可约表达式,则称M为\beta-范式(in \ \beta-normal \ form \ or \ in \ \beta-nf) \\
& (2) \ 若存在一个\beta-范式 N,使得 M =_\beta N , 则称M有一个\beta-范式(has \ a \ \beta-normal \ form \ or \ has \ a \ \beta-nf) \\ & 或者可\beta-标准化(is \ \beta-normalising), 并称N是M的一个\beta-范式.
\end{aligned}
$ `

我们将` $\beta$ `-范式视为` $\lambda$ `-项的输出结果.因此一个` $\beta$ `-范式必然是其自身的一个结果,即一个` $\beta$ `-范式,必然通过0次` $beta$ `-归约得到结果.

` $
\begin{aligned}
& 引理 \ \ \ 当M是\beta-范式时,则 M \twoheadrightarrow_\beta N 意味着 M \equiv N.
\end{aligned}
$ `

> 几个特殊的示例:
>
> - 定义 ` $\Omega := (\lambda x \ . \ x x)(\lambda x \ . \ x x)$ `,则` $\Omega$ `不是` $\beta$ `-范式,因为它本身是一个可约式.而且它不可能归约到一个` $\beta$ `-范式,因为它只能归约到自身.
> - 定义 ` $\Delta := \lambda x . x x x$ `,则有` $\Delta \Delta \rightarrow_\beta \Delta \Delta \Delta \rightarrow_\beta \Delta \Delta \Delta \Delta \rightarrow_\beta \dots $ `. 因此` $\Delta\Delta$ `既不是` $\beta$ ` -范式,也不可归约到一个 ` $\beta$ ` -范式.
> - ` $(\lambda u \ . \ v)\Omega$ `包含了两个可约表达式,因此有两种归约路线,第一种直接归约到` $v$ `,即` $(\lambda u \ . \ v)\Omega =_\beta  v$ `,另一种路线则从` $\Omega$ `出发,无论如何都得不到范式.因此` $(\lambda u \ . \ v)\Omega$ `为可标准化的.

从以上示例中,归约路径之间可能存在关联.

` $
\begin{aligned}
& 定义 \ \ \ (Reduction \ path) \\
& 一个从M出发的有穷的归约路径是一个由有限项组成的序列 N_0,N_1,N_2,\dots,N_n,
\\ & 使得 N_0 \equiv M 且 对于任意的0 \le i \lt n , 有N_i \rightarrow_\beta N_{i+1}.  \\
& 一个从M出发的无穷的归约路径是一个由无限项组成的序列 N_0,N_1,N_2,\dots, \\ & 满足 N_0 \equiv M 且 对任意 i \in \mathbb{N} 均有 N_i \rightarrow_\beta N_{i+1}.
\end{aligned}
$ `

可以定义两个性质良好的子集——只存在一条归约到范式路径的项组成的集合;任意路径均可归约到范式的项集合.

` $
\begin{aligned}
& 定义 \ \ \ (Weak \ normalisation, strong \ normalisation) \\
& (1) \ 若存在一个N是\beta-范式,使得M \twoheadrightarrow_\beta N , 则称M 是弱可标准化. \\
& (2) \ 若从M出发没有无穷归约路径,则称M是强可标准化. 
\end{aligned}
$ `

下面介绍著名的合流性原理,该原理将弱标准化和范式联系起来.

` $
\begin{aligned}
& 定理 \ \ \ (Church-Rosser ; CR ; Confluence) \\
& 假设对于给定的一个项M,有M \twoheadrightarrow_\beta N_1 且 M \twoheadrightarrow_\beta N_2,   则必存在一个项N_3使得 N_1 \twoheadrightarrow_\beta N_3 且 N_2 \twoheadrightarrow_\beta N_3.
\end{aligned}
$ `

` $
\begin{aligned}
& 推论 \ \ \ 
& 假设 M =_\beta N , 则存在L使得M \twoheadrightarrow_\beta L 且 N \twoheadrightarrow_\beta L.
\end{aligned}
$ `

> 提示: 该推论可以通过归纳法进行证明.

` $
\begin{aligned}
& 引理 \\
& (1) \ 如果 N 是 M的\beta-范式,则M \twoheadrightarrow_\beta N \\
& (2) \ 一个\lambda-项有至多一个\beta-范式.
\end{aligned}
$ `

> 以上引理表明如果一个` $\lambda$ `-项存在结果,那么就可以通过` $\beta$ `-归约得到,且这个结果是唯一的.

## 不动点理论 (Fixed Point Theorem)



无类型的lambda演算中,每一个` $\lambda$ `-项都有一个不动点,也就是说对于任意的L,存在一个` $\lambda$ `-项M,使得` $L M =_\beta M$ `.

` $
\begin{aligned}
& 定理 \ \ \  对于任意的L \in \Lambda , 存在 M \in \Lambda , 使得 LM =_\beta M.
\end{aligned}
$ `

> 存在一个Y结合子(` $Y := \lambda y \ . \ (\lambda x \ . \ y (x x))(\lambda x \ . \ y ( x x))$ `),使得对任意的项L,YL是L的一个不动点,即` $L(Y L) =_\beta Y L$ `.因此这个结合子又称为不动点结合子.

**求不动点方法**

是否存在一个` $\lambda$ `-项使得 ` $Mx =_\beta xMx$ `?

定义 ` $L := \lambda y \ . \ (\lambda x \ . \ xyx)$ `,则有 ` $LM \rightarrow_\beta \lambda x \ . \ xMx$ `.因此只要找到$M$使得` $M=_\beta LM$ `即可.使用Y结合子,构造` $M := YL$ `即为不动点.















