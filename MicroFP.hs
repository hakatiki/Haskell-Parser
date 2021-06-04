-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Tim Koree (s2341182)
-- Student 2: Bálint Takáts (s2724448) 

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All





-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll

--eleven := inc (10);
-- f_eleven = Prog [(Function "eleven" [] (Term (Factor ( Identifier "inc" (Term (Factor (Int 10))) [])))) ] 


data Comb = Id String
          | Integer Integer
            deriving Show

data Prog = Prog [Func] 
            deriving Show

data Func = Function String [Comb] Expr
          | Func String Expr
                deriving Show

data Term =  Mult Factor Term 
           | Factor Factor 
             deriving Show

data Order = Greater String
              | Equals String
              | Less String 
                deriving Show

data Expr =  Term Term
           | AddAndSub Term String Expr 
             deriving Show

data Factor = Int Integer
            | Identifier String Expr [Expr]
            | Identifier2 String Expr
            | Identifier3 String
            | If Expr Order Expr Expr Expr 
            | Parens Expr 
              deriving Show

comb :: Parser Comb 
comb = Id <$> identifier <|>
       Integer <$> integer

program :: Parser Prog
program = Prog <$> (manyParsers func)

func :: Parser Func
func = Function <$> identifier <*> (manyParsers comb) <*> ((symbol ":=" *> expr) <* symbol ";") <|>
       Func <$> identifier <*> ((symbol ":=" *> expr) <* symbol ";")

term :: Parser Term
term = Mult <$> factor <*> ((symbol "*") *> term ) <|> 
       Factor <$> factor
       
order :: Parser Order
order = Greater <$> symbol ">" <|>
        Equals <$> symbol "=="<|> 
        Less <$> symbol "<"

expr :: Parser Expr
expr = AddAndSub <$> term <*> (symbol "+" <|> symbol "-") <*> expr <|>
       Term <$> term

factor :: Parser Factor
factor = Int <$> integer <|> 
         Parens <$> parens(expr) <|>
         If <$> (symbol "if" *> symbol "(" *> expr) <*> order <*> (expr <* symbol ")") <*> (symbol "then" *> (braces expr)) <*> (symbol "else" *> (braces expr)) <|>
         (Identifier <$> identifier <*> (symbol "(" *> expr) <*> (manyParsers (symbol "," *> expr)) <* symbol ")" <|> 
         Identifier2 <$> identifier <*> parens(expr) <|>
         Identifier3 <$> identifier)

compile :: String -> Prog
compile x = ret
       where [(ret,_)] = runParser program (Stream x)

-- ================ Test cases ================
testProgram = runParser program (Stream "fourty := twice (double, 10);")
testProgram2 = runParser program (Stream "fib 0 := 1;")
testProgram3 = runParser program (Stream "fib n := if (n < 3) then { 1 } else { fib (n-1) + fib (n-2) };")
testProgram4 = runParser program (Stream "div x y := if (x < y) then { 0 } else { 1 + div ((x-y), y) };")

orderTest1 = runParser order (Stream "<")
orderTest2 = runParser order (Stream ">")
orderTest3 = runParser order (Stream "==")

termTest = runParser term (Stream "12")
termTest2 = runParser term (Stream "12 * 3")
exprTest1  = runParser expr (Stream "10")

testFactor = runParser factor (Stream "123")
testFactor2 = runParser factor (Stream "(12-2)")
testFactor3 = runParser factor (Stream "if (12 == 12) then {40} else {20}")
testFactor4 = runParser factor (Stream "bob (10)")
testFactor5 = runParser factor (Stream "bob (10,21)")
testFactor6 = runParser factor (Stream "if (n < 3) then { 1 } else { fib (n-1) + fib (n-2) }")
testFactor7 = runParser factor (Stream "if (n < 3) then { 1 } else { fib (n-1) + 2 }")

testFunc = runParser func (Stream "a := 12;")
testFunc2 = runParser func (Stream "fourty := twice (double, 10);")
testFunc3 = runParser func (Stream "fib 0 := 1;")
testFunc4 = runParser (manyParsers func) (Stream "fib 0 := 1;")

testExpr = runParser expr (Stream "2+3")
testExpr2 = runParser expr (Stream "fib (2) + fib (2)")

testNeeded = runParser (braces expr) (Stream "{ fib (n-1) + fib (n-2) }")



factorTest1 = runParser factor (Stream "(10+11)")








-- FAILS
funcTest = runParser func (Stream "alma := 10;")















-- [(prettify, _)] = testProgram3



-- pretty :: Prog -> String
-- pretty (Prog []) = ""
-- pretty (Prog (x:xs)) = prettyFunc x ++ " " ++ prettyFuncList xs

-- prettyFuncList ::[Func] -> String
-- prettyFuncList [] = ""
-- prettyFuncList (x:xs) = prettyFunc x ++ " " ++ prettyFuncList xs

-- prettyFunc :: Func -> String
-- prettyFunc (Function str [] expr) = str ++ ":= " ++ prettyExpr expr ++ "; "
-- prettyFunc (Function str middle expr) = str ++ " " ++ prettyCombList middle ++":= " ++ prettyExpr expr ++ "; "

-- prettyCombList :: [Comb] -> String
-- prettyCombList [] = ""
-- prettyCombList (Id x :xs) = " " ++ x ++ " " ++ prettyCombList xs
-- prettyCombList (Integer x :xs) = " " ++ show x ++ " " ++ prettyCombList xs

-- prettyExpr :: Expr->String
-- prettyExpr (Term term) = prettyTerm term
-- prettyExpr (AddAndSub term str expr) = prettyTerm term ++ " " ++ str ++ " " ++ prettyExpr expr

-- prettyExprList :: [Expr] -> String
-- prettyExprList [] = ""
-- prettyExprList (x:xs)= ", " ++prettyExpr x ++ ", " ++ prettyExprList xs 

-- prettyFactor :: Factor -> String
-- prettyFactor (Int i) = show i
-- prettyFactor (Identifier str expr list) = str ++ "( " ++ prettyExpr expr ++ prettyExprList list ++ " )" 
-- prettyFactor (Identifier2 str expr ) = str ++ "( " ++ prettyExpr expr ++ " )"
-- prettyFactor (Identifier3 str  ) = str
-- prettyFactor (If expr1 ord expr2 expr3 expr4) = 
--                             "if( " ++ prettyExpr expr1 ++" "++ prettyOrder ord ++" "++ prettyExpr expr2 ++" ) "++
--                             "then{ " ++ prettyExpr expr3 ++ " } " ++
--                             "else{ " ++ prettyExpr expr4 ++ " }" 
-- prettyFactor (Parens expr) = "( " ++ prettyExpr  expr ++ " )"

-- prettyOrder :: Order -> String
-- prettyOrder (Greater str) = str 
-- prettyOrder (Equals str) = str
-- prettyOrder (Less str) =str
                            
-- prettyTerm :: Term -> String 
-- prettyTerm (Factor fact) =  prettyFactor fact
-- prettyTerm (Mult fact term) = prettyFactor fact ++ " * " ++ prettyTerm term




-- ================ 3.2 

-- pm Fib
       -- Prog [Function "fibonacci" [Integer 0] (Term (Factor (Int 0)))]
       -- Prog [Function "fibonacci" [Integer 1] (Term (Factor (Int 1)))]
       -- Prog [Function "fibonacci" [Id "n"] (AddAndSub (Factor (Identifier2 "fibonacci" (AddAndSub (Factor (Identifier3 "n")) "-" (Term (Factor (Int 1)))))) "+" (Term (Factor (Identifier2 "fibonacci" (AddAndSub (Factor (Identifier3 "n")) "-" (Term (Factor (Int 2))))))))]

-- Fib
       -- Prog [Function "fib" [Id "n"] (Term (Factor (If (Term (Factor (Identifier3 "n"))) (Less "<") (Term (Factor (Int 3))) (Term (Factor (Int 1))) (AddAndSub (Factor (Identifier2 "fib" (AddAndSub (Factor (Identifier3 "n")) "-" (Term (Factor (Int 1)))))) "+" (Term (Factor (Identifier2 "fib" (AddAndSub (Factor (Identifier3 "n")) "-" (Term (Factor (Int 2)))))))))))]

-- sum
       -- Prog [Function "sum" [Integer 0] (Term (Factor (Int 0)))]
       -- Prog [Function "sum" [Id "a"] (AddAndSub (Factor (Identifier2 "sum" (AddAndSub (Factor (Identifier3 "a")) "-" (Term (Factor (Int 1)))))) "+" (Term (Factor (Identifier3 "a"))))]

-- div
       -- Prog [Function "div" [Id "x",Id "y"] (Term (Factor (If (Term (Factor (Identifier3 "x"))) (Less "<") (Term (Factor (Identifier3 "y"))) (Term (Factor (Int 0))) (AddAndSub (Factor (Int 1)) "+" (Term (Factor (Identifier "div" (Term (Factor (Parens (AddAndSub (Factor (Identifier3 "x")) "-" (Term (Factor (Identifier3 "y"))))))) [Term (Factor (Identifier3 "y"))])))))))]

-- twice
       -- Prog [Function "twice" [Id "f",Id "x"] (Term (Factor (Identifier2 "f" (Term (Factor (Identifier2 "f" (Term (Factor (Identifier3 "x")))))))))]

-- double
       -- Prog [Function "double" [Id "a"] (Term (Mult (Identifier3 "a") (Factor (Int 2))))]

-- add

       -- Prog [Function "add" [Id "x",Id "y"] (AddAndSub (Factor (Identifier3 "x")) "+" (Term (Factor (Identifier3 "y"))))]

-- inc
       -- Prog [Func "inc" (Term (Factor (Identifier2 "add" (Term (Factor (Int 1))))))]

-- eleven
       -- Prog [Func "eleven" (Term (Factor (Identifier2 "inc" (Term (Factor (Int 10))))))]

-- fourty
       -- Prog [Func "fourty" (Term (Factor (Identifier "twice" (Term (Factor (Identifier3 "double"))) [Term (Factor (Int 10))])))]

-- main
       -- Prog [Func "main" (Term (Factor (Identifier "div" (Term (Factor (Int 999))) [Term (Factor (Int 2))])))]
