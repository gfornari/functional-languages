{-# OPTIONS_GHC -Wall #-}


{- Exercise 1 -}
-- import GHC.base hiding (((->) r))

-- instance Applicative ((->) r) where  
--     pure x = (\_ -> x)
--     g <*> m = \x -> g x (m x)

-- instance Monad ((->) r) where  
--     return x = \_ -> x  
--     f >>= k = \w -> k (f w) w  



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

{- The >>= operator takes a (Expr a) value and a function of type (a -> Expr
b) and apply the function to the (Expr a).

The do notation used in the evalInt example is the same as:

evalInt ex1 >>= \a ->
evalInt ex2 >>= \b ->
return (a + b)
-}

evalInt :: Expr Int -> Expr Int
evalInt (Var a) = Var a
evalInt (Val n) = Val n
evalInt (Add ex1 ex2) = do
    a <- evalInt ex1
    b <- evalInt ex2
    return (a + b)

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


-- main :: IO ()
-- main = putStrLn "Nothing to show here..."