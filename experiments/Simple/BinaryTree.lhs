\section{Serialized Binary Trees}
\ignore{
\begin{code}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Simple.BinaryTree where
import Prelude.Linear
import Data.Word
import Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Builder as B
import Data.Binary (get, put, Binary) -- encode, decode, getWord8
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (execPut)
import Data.Kind
import qualified Data.Monoid as Monoid-- HACK to make app work below, should define linear monoid for builder
import Unsafe.Linear
\end{code}
}


A ``needs'' cursor requires a list of fields be written to the
bytestream before the data is fully initialized.  Once it is, a
value of the (second) type parameter can be extracted.

\begin{code}
newtype Needs (l :: [Type]) t = Needs Builder
\end{code}

A ``has'' cursor is a pointer to a series of consecutive,
serialized values.  It can be read multiple times.

\begin{code}
newtype Has (l :: [Type]) = Has ByteString
  deriving Show
\end{code}

A packed value is very like a singleton Has cursor.  It
represents a dense encoding of a single value of the type |a|.

\begin{code}
newtype Packed a = Packed ByteString
  deriving (Show,Eq)
\end{code}

\ignore{
\begin{code}
instance Semigroup Builder where
  (<>) = toLinear2 Monoid.mappend
instance Monoid Builder where
  mempty = Monoid.mempty
data NotUr a where NotUr :: a #-> NotUr a
unsafeUr :: a #-> Ur a
unsafeUr x = coerce (NotUr x)
\end{code}
}

Write a value to the cursor.  Write doesn't need to be linear in
the value written, because that value is serialized and copied.

\begin{code}
writeC :: Binary a => a -> Needs (a ': rst) t #-> Needs rst t
writeC a (Needs bld1) = Needs (bld1 <> execPut (put a))
\end{code}

Reading from a cursor scrolls past the read item and gives a
cursor into the next element in the stream:

\begin{code}
readC :: Binary a => Has (a ': rst) #-> (Ur a, Has rst)
readC = toLinear readC'
  where readC' (Has bs) =
          case runGetOrFail get bs of
            Left (_,_,err) -> error ("internal error: "++err)
            Right (remain,_num,a) -> (Ur a, Has remain)
\end{code}

Safely ``cast'' a |Has| cursor to a |Packed| value.

\begin{code}
fromHas :: Has '[a] #-> Packed a
fromHas (Has b) = Packed b
\end{code}

Safely ``cast'' a |Packed| value to a |Has| cursor.
\begin{code}
toHas :: Packed a #-> Has '[a]
toHas (Packed b) = Has b
\end{code}

\begin{code}
unsafeCastNeeds :: Needs l1 a #-> Needs l2 a
unsafeCastNeeds (Needs b) = (Needs b)

unsafeCastHas :: Has l1 #-> Has l2
unsafeCastHas (Has b) = (Has b)
\end{code}

\begin{code}
splitC :: forall x y z b t.
           Has (x ': b)
       #-> Word8
       ->  Word8
       ->  (Has y #-> t)
       ->  (Has z #-> t)
       ->  t
splitC = toLinear splitC'
  where splitC' :: Has (x ': b)
                -> Word8
                -> Word8
                -> (Has y #-> t)
                -> (Has z #-> t)
                -> t
        splitC' (Has bs) ty1 ty2 f1 f2 =
          case runGetOrFail get bs of
            Left (_,_,err) -> error ("internal error: "++err)
            Right (rst,_num,tag) ->
              if tag == ty1 then f1 (unsafeCastHas $ Has rst)
              else if tag == ty2 then f2 (unsafeCastHas $ Has rst)
              else error ("invalid tag: " ++show tag)
\end{code}

\begin{code}
finish :: Needs '[] a #-> Ur (Has '[a])
finish (Needs bs) = unsafeUr $ Has (toBS bs)
\end{code}

\begin{code}
withOutput :: forall a b. (Needs '[a] a #-> Ur b) #-> Ur b
withOutput fn =
    force $ fn (Needs mempty)
  where
    force :: Ur b #-> Ur b
    force (Ur x) = Ur x
\end{code}

\begin{code}
unsafePackedToBinary :: Packed a #-> ByteString
unsafePackedToBinary (Packed bs) = bs

unsafeBinaryToPacked :: ByteString #-> Packed a
unsafeBinaryToPacked bs = Packed bs

toBS :: Builder #-> ByteString
toBS = toLinear B.toLazyByteString
\end{code}
