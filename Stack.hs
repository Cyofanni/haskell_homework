import Control.Applicative
import Control.Monad


type Stack = [Char]

pop :: Stack -> (Char, Stack)
pop (x:xs) = (x, xs)

push :: Char -> Stack -> ((), Stack)
push x xs = ((), x:xs)

check_par :: [Char] -> Stack -> Bool
check_par [] [] = True
check_par [] _   = False
check_par (x:xs) st = if x == '(' then
		       let ((), st') = (push ')' st) in
			   (check_par xs st')
		      else if x == ')' then
			let (y, st') = pop st in
			   if y == ')' then
				check_par xs st' 
		           else False
		      else (check_par xs st)

-- **********************************************************

type Stato = [Char]
newtype ST a = S(Stato -> (a, Stato))

pop1:: ST Char
pop1 = S(\(x:xs) -> (x, xs))

push1:: ST ()
push1 = S(\xs -> ((), (')':xs)))

app:: ST a -> Stato -> (a, Stato)
app (S st) x = st x

instance Functor ST where
	fmap f st = S(\x -> let (s, s1) = app st x in (f s, s1))

instance Applicative ST where
	pure x = S(\s -> (x,s))
	stf <*> stx = S(\s -> let (f,s1) = app stf s
				  (x, s2) = app stx s1
			      in (f x, s2))

instance Monad ST where
	st >>= f = S(\s -> let (x, s1) = app st s in app (f x) s1)


-- **EX

check_par1 :: [Char] -> ST Bool
-- the stack must be empty for the check to success
check_par1 [] = S(\s -> if s == "" then (True, s) else (False, s))
check_par1 ('(':xs) = do push1
                         check_par1 xs
check_par1 (')':xs) = do pop1
                         check_par1 xs
check_par1 (x:xs) = check_par1 xs
