import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char   -- P(\inp -> (Char, String))
item = P(\inp -> case inp of
	     	 []     -> []
		 (x:xs) -> [(x, xs)])


instance Functor Parser where 
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P(\inp -> case parse p inp of 
			[]         -> []
	  	        [(v, out)] -> [(g v, out)])


instance Applicative Parser where
   --pure :: a -> Parser a
   pure v    = P(\inp -> [(v, inp)])
   -- <*> :: Parser(a -> b) -> Parser a -> Parser b
   pg <*> px = P(\inp -> case parse pg inp of
		 	[]         ->  []
			[(g, out)] ->  parse (fmap g px) out)

three :: Parser (Char, Char)
three =  pure g <*> item <*> item <*> item
	 where g x y z = (x, z)


instance Monad Parser where
   return  = pure
   -- >>= :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P(\inp -> case parse p inp of
			[]         -> []
	  		[(v, out)] -> parse (f v) out)

three_m :: Parser (Char, Char)
three_m  =  do
		x <- item
		item
		y <- item
		return (x, y) 


instance Alternative Parser where 
  -- empty :: Parser a
    empty = P (\inp -> [])
  --(<|>) :: Parser a -> Parser a -> Parser a
    p <|> q  =  P(\inp -> case parse p inp of
			  []  ->  parse q inp
			  [(v, out)] -> [(v, out)])	 



sat :: (Char -> Bool) -> Parser Char
sat p = do 
	   x <- item
	   if (p x) then return x
	   else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do
		  char x
		  string xs
		  return (x:xs)

ident :: Parser String
ident = do
	  x <- lower
	  xs <- many alphanum		
	  return (x:xs)


-- FIXME: write a new ident that can discard keywords
--        so far identp returns [("","")] (should be []) when it finds a keyword
{-identp :: Parser String
identp = do
	  x <- lower
	  xs <- many alphanum	
	  let s = x:xs
	  case s of 
		"let" -> return []
		{-"in" -> return  [] 	
		"lambda" -> 	
		"if" -> 
		"then" -> 
		"cons" -> 
		"head" -> 
		"tail" -> 
		"eq" -> 
		"leq" -> 
		"null" -> -} 			
-}

nat :: Parser Int
nat = do
	xs <- some digit 
	return (read xs)
	
space :: Parser ()
space = do
	  many (sat isSpace)
	  return ()

int :: Parser Int
int = do
	char '-' 
	n <- nat
	return (-n)
	<|> nat

token :: Parser a -> Parser a
token p = do 
	    space
	    v <- p	
	    space
	    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
	symbol "["
	n <- natural
	ns <- many (do 
                     symbol "," 
	             natural)
	symbol "]"
	return (n:ns)

-- Context Free Grammar for expressions implementation
expr :: Parser Int
expr = do
         t <- term
         do
            symbol "+"
            e <- expr
            return (t + e)
            <|> return t

term :: Parser Int
term = do
        f <- factor
        do
            symbol "*"
            t <- term
            return (f * t)
            <|> return f

factor :: Parser Int
factor = do
           symbol "("
           e <- expr
           symbol ")"
           return e
           <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
                [(n, [])]  ->  n
                [(_, out)] ->  error ("unused input: " ++ out)
                []         ->  error "invalid input"



-- ****Homework assignment (LKC parser)****

data LKC = VAR String | NUM Int | NULL | ADD LKC LKC |
           SUB LKC LKC | MULT LKC LKC | DIV LKC LKC |
           EQ LKC LKC | LEQ LKC LKC | H LKC | T LKC |
           CONS LKC [LKC] {-modified from CONS LKC LKC-} | IF LKC LKC LKC | LAMBDA [LKC] LKC |
           CALL LKC [LKC] | LET LKC [(LKC, LKC)] | LETREC LKC [(LKC, LKC)]
           deriving (Show, Eq)

{-
  The project consists in writing a parser for the given grammar. 
  This parser must compute, for any given input string that contains 
  a program generated by the grammar, a tree representing the structure 
  of the program.
-}

{-
  LISPKIT CONTEXT FREE GRAMMAR (keywords in '')
     1.	Prog := 'let' Bind 'in' exp 'end' | 'letrec' Bind 'in' Exp 'end'
     2.	Bind := var = Exp ('and' Bind | EMPTY)	 
     3. Exp  := Prog | 'lambda' (Seq_Var) Exp | Expa | OPP (Seq_Var) |
	   'if' Exp 'then' Exp 'else' Exp
     4. Expa := Term (OPA Expa | EMPTY)
     5. Term   := Factor (OPM Term | EMPTY)
     6. Factor := var (Y | EMPTY) | integer | 'null' (Expa)
     7. Y := () | (Seq_Exp)
     8. OPA :=  '+' | '-'
     9. OPM :=  '*' | '/'
     10. OPP := 'cons' | 'head' | 'tail' | 'eq' | 'leq'
     11. Seq_Exp := Exp (, Seq_Exp | EMPTY)
     12. Seq_Var := var Seq_Var | EMPTY
-}

-- IDEA:non-terminal -> haskell function

--1. Prog := 'let' Bind 'in' exp 'end' | 'letrec' Bind 'in' Exp 'end'
prog :: Parser LKC
prog = do
	symbol "let"
	b <- bind
	symbol "in"
	e <- Main.exp
	symbol "end"
	return (LET e b)
	<|> do 
	     symbol "letrec"
	     b <- bind
	     symbol "in"
	     e <- Main.exp
	     symbol "end"	
	     return (LETREC e b)


--2. Bind := 'var' = Exp ('and' Bind | EMPTY)
-- NOTE: the type has been changed from 'Parser LKC' to this new one, may cause issues 
-- with the other functions
bind :: Parser [(LKC, LKC)]
bind = do
	 i <- identifier
	 symbol "="
	 e <- Main.exp
	 do
	   symbol "and"
	   bs <- bind
	   return (((VAR i),e):bs)
	   <|> return [((VAR i),e)]
	    
--3. Exp  := Prog | 'lambda' (Seq_Var) Exp | Expa | OPP (Seq_Var) | 'if' Exp 'then' Exp 'else' Exp
-- NOTE: Seq_Var is a kind of "x y z" without any commas!
exp :: Parser LKC
exp =  do
	  p <- prog 
       	  return p
       -- choose the prefix operator
       -- NOTE: the current grammar doesn't allow this (taken from the pdf): cons(a,cons(b, null)),
       --       because Seq_Var is a simple list of identifiers
       <|> do
	  symbol "cons"
	  symbol "("
	  i <- identifier
          ss <- seqvar       -- 'seqvar' returns a Parser [LKC]
          symbol ")"	
  	  return (CONS (VAR i) ss)             
	  <|> do
	       symbol "head"
	       i <- identifier
	       return (H (VAR i))
	  <|> do
	       symbol "tail"
	       i <- identifier
	       return (T (VAR i))
	  <|> do
	      --FIXME how to handle "eq" and "leq"? "eq Seq_Var" doesn't make sense
	       symbol "eq"
	       symbol "("
	       i1 <- identifier
	       i2 <- identifier
	       symbol ")"
	       return (Main.EQ (VAR i1) (VAR i2))
	  <|> do
	      --FIXME how to handle "eq" and "leq"? "eq Seq_Var" doesn't make sense
	       symbol "leq"
	       symbol "("
	       i1 <- identifier
	       i2 <- identifier
	       symbol ")"
	       return (LEQ (VAR i1) (VAR i2))
	  

       <|> do
            --'lambda' (Seq_Var) Exp 
	    symbol "lambda"
	    symbol "("
	    s <- seqvar
	    symbol ")"
	    e <- Main.exp
	    return (LAMBDA s e)
	    <|> do
	  -- 'if' Exp 'then' Exp 'else' Exp
               symbol "if"
	       e1 <- Main.exp     -- the name 'exp' is ambiguous 
	       symbol "then"
	       e2 <- Main.exp
	       symbol "else"
	       e3 <- Main.exp
               return (IF e1 e2 e3)
        <|> do 
             --Expa
	       e <- expa
	       return e 

--4. Expa := Term (OPA Expa | EMPTY)
expa :: Parser LKC
expa = do
         t <- termp
         do
   	    -- don't know whether '+' or '-'
            symbol "+"
            e <- expa
   	    return (ADD t e)
            <|> do 
		  symbol "-"
	          e1 <- expa
	          return (SUB t e1)
	    <|> return t
       
          

--5. Term := Factor (OPM Term | EMPTY)
termp :: Parser LKC
termp = do
         t <- factorp
         do
            -- don't know whether '*' or '/'
            symbol "*"
            t1 <- termp
            return (MULT t t1)
            <|> do
		 symbol "/"
                 t1 <- termp 
                 return (DIV t t1)
            <|> return t
 	
--6. Factor := var (Y | EMPTY) | integer | 'null' (Expa)
-- NOTE: what's the meaning of that 'null' in the production?
factorp :: Parser LKC
factorp = do
	   i <- identifier
	   do -- may be a function call
	         ip <- ipsilon
	         return (CALL (VAR i) ip)
	   	 <|> return (VAR i)
           <|> do 
                   int <- integer
                   return (NUM int)
           <|> do
	         symbol "("
		 e <- expa
	         symbol ")"
		 return e
	   
	  {- <|> symbol "null"
	       return NULL 
  	  -}
	   
-- 7. Y := () | (Seq_Exp)
-- NOTE: the type has been changed from 'Parser LKC' to this new one, may cause issues 
-- with the other functions
ipsilon :: Parser [LKC]
ipsilon = do
	    symbol "("
	    symbol ")"
	    return [NULL]
	  <|> do
               symbol "("
	       s <- seqexp
	       symbol ")"
	       return s

--11. Seq_Exp := Exp (, Seq_Exp | EMPTY)
-- NOTE: the type has been changed from 'Parser LKC' to this new one, may cause issues 
-- with the other functions
seqexp :: Parser [LKC]
seqexp = do
	    e <- Main.exp
	    do 
		symbol ","
		es <- seqexp
		return (e:es)
	        <|> return [e]
		  

-- 12. Seq_Var := var Seq_Var | EMPTY
-- NOTE: the type has been changed from 'Parser LKC' to this new one, may cause issues 
-- with the other functions
seqvar :: Parser [LKC]
seqvar = do
	   i <- identifier
	   do 
              is <- seqvar
              return ((VAR i):is)          
              <|> return ([VAR i])


-- starter function, should be based on "eval"
parseProgram :: String -> LKC
parseProgram xs = case (parse prog xs) of
                [(n, [])]  ->  n
                [(_, out)] ->  error ("unused input: " ++ out)
                []         ->  error "invalid input"

