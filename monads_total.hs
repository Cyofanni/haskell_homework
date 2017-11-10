-- Exercises on Monads 12/19/16
-- **************************************************************
--EX 1) Define ad instance of the Monad class for the type (a ->).
import Control.Applicative
import Control.Monad

{- NOTE:: 'a' matches the left operand of '->',
   thus, f b === (a -> b)
-}

instance Functor ((->) a) where
-- fmap :: (x -> y) -> f x -> f y
-- fmap :: (b -> c) -> (a -> b) -> (a -> c)
fmap g f =  g . f
-- alternative version: map = (.)

instance Applicative ((->) a) where
-- pure  :: b -> (a -> b)
pure = const
-- (<*>) :: f(x -> y) -> f x -> f y
-- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
g <*> h = \x -> g x (h x) 

instance Monad ((->) a) where
-- return :: b -> (a -> b)
return = Main.pure
-- (>>=)  ::  f w -> (w -> f t) -> f t
-- (>>=)  :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
g >>= h   =  \x -> h (g x) x

-- ************************************************************

--EX 2) 
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



--EX 3)

type State = Int
newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x


-- NEW Functor definition, using the 'do notation'
instance Functor ST where
--fmap :: (a -> b) -> (ST a) -> (ST b)
fmap g st = do
	      x <- st
	      Main.return (g x)

--NEW Applicative definition, using the 'do notation'

instance Applicative ST where
pure x = S (\s -> (x, s))
--(<*>) :: ST (a -> b) -> ST a -> ST b
stf <*> stx = do
		x <- stx
		f <- stf
		Main.return (f x)


instance Monad ST where
return = Main.pure
-- (>>=) :: ST a -> (a -> ST b) -> ST b
st >>= f   =   S(\s ->
               let (x, s') = app st s in app (f x) s')






{-data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n     = (Leaf n, n + 1)
rlabel (Node l r) n   = (Node l' r', n'') where
                        (l', n')  = rlabel l n
                        (r', n'') = rlabel r n'


fresh :: ST Int
fresh = S (\n -> (n, n + 1))

--alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)    = Main.pure Leaf Main.<*> fresh
alabel (Node l r)  = Main.pure Node Main.<*> alabel l Main.<*> alabel r


--mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)    =      fresh Main.>>= (\n
                          -> Main.return (Leaf n))
mlabel (Node l r)  = 	mlabel l Main.>>= (\x ->
			mlabel r Main.>>= (\y ->
			Main.return (Node x y)))
                        --l' <- mlabel l
                        --r' <- mlabel r
                        --Main.return (Node l' r')


tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
-}
