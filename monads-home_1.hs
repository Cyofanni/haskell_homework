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
