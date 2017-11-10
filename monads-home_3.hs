import Control.Applicative
import Control.Monad

type State = Int
newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

{- OLD Functor definition
instance Functor ST where
--fmap :: (a -> b) -> (ST a) -> (ST b)
fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))
-}
-- NEW Functor definition, using the 'do notation'
instance Functor ST where
--fmap :: (a -> b) -> (ST a) -> (ST b)
fmap g st = do
	      x <- st
	      Main.return (g x)

{- OLD Applicative definition
instance Applicative ST where
--pure :: a -> ST a
pure x = S (\s -> (x, s))
--(<*>) :: ST (a -> b) -> ST a -> ST b
stf <*> stx = S (\s ->
        let (f, s') = app stf s
            (x, s'') = app stx s' in (f x, s''))
-}
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


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

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

