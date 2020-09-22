\section{Some Pure Functions}

This was adapted from the documentation of @linear-base@.

We have simple linear functions and simple linear data structures that
illustrate the basic concepts of how the type checker of GHC with linear
types behaves. The goal of this is to be a ridiculously simple tutorial
on the basics of linear types.

\ignore{
\begin{code}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
module Simple.Pure where
\end{code}
}

\subsection{Simple linear functions}

A linear function simply ``consumes/uses'' its argument exactly once.

Giving a more precise idea of this is tricky, so first we present a bunch of
examples. You should try to get a sense of the arithmatic of how many times
an argument in a function is used. In other words, you should have some idea
of a counting function in your head, that can take some haskell function
|f :: A -> B| and give a natural number as to the number of times the argument
of |f| is used in the body.

\begin{code}
linearIdentity :: a #-> a
linearIdentity x = x
\end{code}

Definition: We say the argument of a function is linear if the arrow
that follows it is linear.

Here, the argument is present exactly once in the body and is
consumed exactly once.

In Linear Haskell, |a #-> a| is written as \verb!a #-> a!.

\begin{code}
linearSwap :: (a,a) #-> (a,a)
linearSwap (x,y) = (y,x)
\end{code}

Here, the argument is decomposed by the tuple data constructor into two
pieces. Since the whole first argument is linear, the tuple constructor
in linear haskell is linear -- i.e., notice the type of the
constructor:

\begin{spec}
(,) :: a #-> b #-> (a,b)
\end{spec}

Now, this does not mean that if we have some non-linear function with
an argument |(a,b)| that |a| and |b| have to be consumed exactly once.

Definition: A linear arrow in a data constructor merely signifies that the argument to
the constructor that preceedes that arrow must be used linearly if the input
to a linear function is this data type with this constructor.

Here, since |(,) x y| was the first input to linearSwap, which is a
linear function (meaning the first input is linear), the components |x|
and |y| must be used linearly. Indeed, we see the |x| and |y| are each
used exactly once.

With a non-linear function, this is not the case; a non-linear function
|f :: (x,y) -> B| does not have to use the |x| and |y| linearly.
Consider the next function as an example.

\begin{code}
nonLinearSubsume :: (a,a) -> (a,a)
nonLinearSubsume (x,_) = (x,x)
\end{code}

This function is not linear on its argument and in fact could not have a
linear arrow. If it did, this file would not compile. Why is this?  Well,
the first argument would be linear, and the constructor is linear in both
components.

\begin{spec}
(,) :: a #-> b #-> (a,b)
\end{spec}

Again, in a linear function, this means the |a| and |b| must be used
linearly.

Yet, in the body of the function, |a| is used twice and |b| is used
zero times.

\begin{code}
linearPairIdentity :: (a,a) #-> (a,a)
linearPairIdentity (x,y) = (x,y)
\end{code}

Here, notice that |(a,a)| is linear, and since |(,)| is linear
on both arguments, both |a|s must be used linearly, and indeed
they are. Each is consumed exactly once in a linear input
to the constructor |(,) :: a #-> b #-> (a,b)|.

Notice the general pattern: we consumed |(a,b)| linearly by pattern matching
into @Constructor arg1 ... argn@ and consumed the linearly bound arguments
of the constructor linearly by giving them as arguments to some other
constructor that is linear on the appropreate arguments.

\begin{code}
linearIdentity2 :: a #-> a
linearIdentity2 x = linearIdentity x
\end{code}

Of course, another way to use an input linearly (or a component of an input)
is to pass it to a linear function.  Here, since linearIdentity is linear,
we can be sure the term (linearIdentity x) consumes x exactly once.  Hence,
all of linearIdentity2 consumes the input x exactly once.

If we replaced it with the original |id|, this would fail to type check
because |id| has the non-linear type |a -> a|.  Thus, GHC isn't sure
that |id| uses its input exactly once.  If |id| doesn't use its input
exactly once, then linearIdentity2 won't use its input exactly once,
violating it's type signature.

Now, this does not mean that merely using a linear function makes the
use of a variable linear. For instance, both of the two functions below
use their input exactly twice.

\begin{code}
nonLinearPair :: a -> (a,a)
nonLinearPair x = (linearIdentity x, linearIdentity x)
\end{code}

\begin{code}
nonLinearPair2 :: a -> (a,a)
nonLinearPair2 x = (x, linearIdentity x)
\end{code}


The function below uses its input exactly thrice.

\begin{code}
nonLinearTriple :: a -> (a,(a,a))
nonLinearTriple x = (linearIdentity x, linearIdentity (nonLinearPair2 x))
\end{code}

With several examples in hand, we can now give a more precise way of
constructively checking that an argument is ``consumed exactly once''.
Here's a rough (good enough most of the time) definition:

Let |f :: A #-> B|.  Suppose that we don't have the identity, |f x = x|
which is trivially linear. Then, the thunk |(f x)| is basically a tree
composed of function applications, data constructors and case
statements.

We say |f| is linear if for any thunk |(f x)| if:

\begin{enumerate}
\item
  If |x| is not a function and is not deconstructed in a case statement:
  \begin{itemize}
  \item case (a): |(f x)| is some function application |(func t1 ... tn)| or
            a constructor |(Constr t1 ... tn)|.
    Either (i) exactly one of the |t_i| is |x| and the function |func| or
    constructor |Constr| is linear on its ith argument, or (ii) |x| is used
    exactly once in exactly one |t_i|.
  \item case (b): |(f x)| is a case statement |(case s of [a_i -> t_i])|,
     |x| is used exactly once in \emph{each} |t_i| (and not at all in |s|).
  \end{itemize}
\item
  If |x| is a function, then for some argument |u|, |(f u)| is used
  exactly once.
\item
  If |x| is deconstructed into |(Constructor t1 ... tn)| in a case statement
  then whatever pieces |t_i| that are bound linearly by the constuctor,
  must be consumed exactly once.
\end{enumerate}

\begin{code}
regularIdentity :: a -> a
regularIdentity x = linearIdentity x
\end{code}
Of course, the fact that a function is linear makes no difference in
non-linear functions. So, non-linear functions can call linear
functions willy-nilly and they will work as expected. (Obviously,
the converse is false, which is kind of the point of linear types.)
To state the obvious, linear functions are regular functions but not all
functions are linear functions.

\begin{code}
(#.) :: (b #-> c) -> (a #-> b) -> (a #-> c)
g #. f = \a -> g (f a)
\end{code}

\begin{code}
linearCompose :: (a,a) #-> (a,a)
linearCompose = linearIdentity #. linearSwap
\end{code}

Above, we compose two linear functions and write a linear version of
|(.)|.  Here, as before, it is critical that we are composing linear
functions.  Notice that we cannot write a function of the following
type:

\begin{spec}
(##.) :: (b -> c) -> (a #-> b) -> (a #-> c)
\end{spec}


\subsection{Linear functions with user data types}


As we've seen, we can consume linearly bound inputs into data types if
the constructor has a linear arrow before the input.

\begin{code}
data LinearHolder a where
  LinearHolder :: a #-> LinearHolder a
\end{code}

\begin{code}
linearHold :: a #-> LinearHolder a
linearHold x = LinearHolder x
\end{code}


Note that if the constructor |LinearHolder| did not have the |#->| then
|linearHold| would not compile, because then you could use the value
non-linearly.

\begin{code}
linearHoldExtract :: LinearHolder a #-> a
linearHoldExtract (LinearHolder x) = x

linearIdentity3 :: a #-> a
linearIdentity3 = linearHoldExtract #. linearHold
\end{code}


For clarity, we include an example of using such linear constructors.
Here, |linearHoldExtract| must use the inner component linearly.
Therefore, it's impossible to implement the following function:

\begin{spec}
linearHoldPair :: LinearHolder a #-> (a,a)
linearHoldPair (LinearHolder x) = (x,x)
\end{spec}

In fact, we have the following equivalence: |(LinearHolder a  #-> b)| $\cong$ |(a #-> b)|

\begin{code}
data LinearHolder2 where
  LinearHolder2 :: a #-> b -> LinearHolder2
\end{code}

\begin{code}
linearHold' :: a #-> LinearHolder2
linearHold' x = LinearHolder2 x "hello"
\end{code}

We can have constructors with mixed arrows, of course. Here, this means only the
 first value is bound linearly. The following fails to type check:

\begin{spec}
linearHold' x = LinearHolder2 "hi" x -- fails to type check
\end{spec}

\begin{code}
data ForcedUnlinear a where
  ForcedUnlinear :: a -> ForcedUnlinear a
\end{code}

\begin{code}
forcedLinearPair :: ForcedUnlinear a #-> (a,a)
forcedLinearPair (ForcedUnlinear x) = (x,x)
\end{code}

Above we define a data type ForcedUnlinear which does not use the
linear arrow to hold it's argument. This means that even if an input of
type |ForcedUnlinear a| is linear, the component does not have to be.
Hence, we can write the function above but could not write something
the following type:

\begin{spec}
linearPair :: a #-> (a,a)
\end{spec}

\begin{code}
demote :: (ForcedUnlinear a #-> b) -> (a -> b)
demote f x = f (ForcedUnlinear x)

promote :: (a -> b) -> (ForcedUnlinear a #-> b)
promote f (ForcedUnlinear x) = f x
\end{code}


Another way of saying this is the following equivalence proven by the
two functions above:
|(ForcedUnlinear a  #-> b)| $\cong$ |(a -> b)|


In the Linear Haskell POPL '18 paper, this datatype is called
|Unrestricted|.
