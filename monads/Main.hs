{-# OPTIONS_GHC -Wall #-}


{- Exercise 1
Define an instance of the Monad class for the type (a ->) .
Remember that one has to write, instance Monad ((->)a) where...

Note: This code is comment otherwise the the module doee not compile due to
duplicate instance declarations.


instance Applicative ((->) r) where  
    pure x = (\_ -> x)
    g <*> m = \x -> g x (m x)

instance Monad ((->) r) where  
    return x = \_ -> x  
    f >>= k = \w -> k (f w) w  
-}

{- Exercise 2
Given the following type of expressions

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

that contains variables of some type a, show how to make this type into
instances of Functor, Applicative and Monad classes. With the aid of an
example, explain what the >>= operator for this type does. -}
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap _ (Val n) = Val n
    fmap f (Add a b) = Add (fmap f a) (fmap f b)

instance Applicative Expr where
    pure = Var
    (Var a) <*> g = fmap a g
    (Val n) <*> _ = Val n
    (Add a b) <*> g = Add (a <*> g) (b <*> g)

instance Monad Expr where
    (Var a) >>= f = f a
    (Val n) >>= _ = Val n
    (Add a b) >>= f = Add (a >>= f) (b >>= f)

{-
The >>= operator takes a (Expr a) value and a function of type (a -> Expr b)
and apply the function to the (Expr a). The values (i.e. the data constructed
with Val) are not affected by the functions.
-}

{- In the example1, we append the " world!" string only to the Var, without
changing the Val.

The output of example1 is:

Add (Add (Var "Hello world!") (Var "Hello world!")) (Val 42)
-}
example1 :: Expr String
example1 = (Add (Add (Var "Hello") (Var "Hello")) (Val 42)) >>= (\x -> pure (x ++ " world!"))

{- In the example2, we count the length if the strings and replace the value
of each Var with a Maybe Int, again without changing the Val. The
lengthIfNotZero function symply returns Nothing if the string is empty, Just
x, with x value of the function call "length" on the input string, otherwise.

The output of example2 is:

Add (Add (Var (Just 5)) (Var Nothing)) (Val 42)
-}
lengthIfNotZero :: String -> Maybe Int
lengthIfNotZero [] = Nothing
lengthIfNotZero s = Just (length s)

example2 :: Expr (Maybe Int)
example2 = (Add (Add (Var "Hello") (Var "")) (Val 42)) >>= (\x -> pure (lengthIfNotZero x))

{- Exercise 3
Definition of the Functor and Applicative instances of ST, using the do
notation. -}
type State = [Char]

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    fmap g st = do
        s <- st
        return (g s)

instance Applicative ST where
    pure x = S(\s -> (x, s))
    stf <*> stx = do
        f <- stf
        x <- stx
        return (f x)

instance Monad ST where
    st >>= f = S(\s ->
        let (x,s') = app st s in app (f x) s')