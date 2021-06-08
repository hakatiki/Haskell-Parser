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
import Data.Map (Map)
import qualified Data.Map as Map





-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll


-- ======================== EDSL ==========================

-- FP3.1
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

data Order = Greater String -- We save the string so we have an easier time when we want to print it in pretty
              | Equals String
              | Less String 
                deriving Show

data Expr =  Term Term
           | AddAndSub Term String Expr -- We save the String so we can combine Add and Sub into AddAndSub
             deriving Show

data Factor = Int Integer
            | Identifier String Expr [Expr]
            | Identifier2 String Expr
            | Identifier3 String
            | If Expr Order Expr Expr Expr 
            | Parens Expr 
              deriving Show


-- ======================== EDSL Parsers ==========================

-- FP4.1
comb :: Parser Comb 
comb = Id <$> identifier <|>
       Integer <$> integer
-- We can use manyParsers as the * operator
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

-- Int, Parens and If are straightforward but order matters a lot with identifiers.
-- We try to parse as many things as possible : identifier (identifier, ...)
-- If that fails we try to parse less stuff: identifier (identifier)
-- Else we parse an identifier
factor :: Parser Factor
factor = Int <$> integer <|> 
         Parens <$> parens(expr) <|>
         If <$> (symbol "if" *> symbol "(" *> expr) <*> order <*> (expr <* symbol ")") <*> (symbol "then" *> (braces expr)) <*> (symbol "else" *> (braces expr)) <|>
         (Identifier <$> identifier <*> (symbol "(" *> expr) <*> (manyParsers (symbol "," *> expr)) <* symbol ")" <|> 
         Identifier2 <$> identifier <*> parens(expr) <|>
         Identifier3 <$> identifier)


-- ======================== Compile ==========================    

-- FP4.2
compile :: String -> Prog
compile x = ret
       where [(ret,_)] = runParser program (Stream x)


-- ======================== Pretty ==========================

-- FP3.3
-- Recursibely prettifies the parsed program and concats the strings that are returned
pretty :: Prog -> String
pretty (Prog []) = ""
pretty (Prog (x:xs)) = prettyFunc x ++ " " ++ prettyFuncList xs

prettyFuncList ::[Func] -> String
prettyFuncList [] = ""
prettyFuncList (x:xs) = prettyFunc x ++ " " ++ prettyFuncList xs

prettyFunc :: Func -> String
prettyFunc (Function str [] expr) = str ++ ":= " ++ prettyExpr expr ++ "; "
prettyFunc (Function str middle expr) = str ++ " " ++ prettyCombList middle ++":= " ++ prettyExpr expr ++ "; "

prettyCombList :: [Comb] -> String
prettyCombList [] = ""
prettyCombList (Id x :xs) = " " ++ x ++ " " ++ prettyCombList xs
prettyCombList (Integer x :xs) = " " ++ show x ++ " " ++ prettyCombList xs

prettyExpr :: Expr->String
prettyExpr (Term term) = prettyTerm term
prettyExpr (AddAndSub term str expr) = prettyTerm term ++ " " ++ str ++ " " ++ prettyExpr expr

prettyExprList :: [Expr] -> String
prettyExprList [] = ""
prettyExprList (x:xs)= ", " ++prettyExpr x ++ ", " ++ prettyExprList xs 

prettyFactor :: Factor -> String
prettyFactor (Int i) = show i
prettyFactor (Identifier str expr list) = str ++ "( " ++ prettyExpr expr ++ prettyExprList list ++ " )" 
prettyFactor (Identifier2 str expr ) = str ++ "( " ++ prettyExpr expr ++ " )"
prettyFactor (Identifier3 str  ) = str
prettyFactor (If expr1 ord expr2 expr3 expr4) = 
                            "if( " ++ prettyExpr expr1 ++" "++ prettyOrder ord ++" "++ prettyExpr expr2 ++" ) "++
                            "then{ " ++ prettyExpr expr3 ++ " } " ++
                            "else{ " ++ prettyExpr expr4 ++ " }" 
prettyFactor (Parens expr) = "( " ++ prettyExpr  expr ++ " )"

prettyOrder :: Order -> String
prettyOrder (Greater str) = str 
prettyOrder (Equals str) = str
prettyOrder (Less str) = str
                            
prettyTerm :: Term -> String 
prettyTerm (Factor fact) =  prettyFactor fact
prettyTerm (Mult fact term) = prettyFactor fact ++ " * " ++ prettyTerm term


-- ======================== Eval + Pattern Match ==========================

-- FP3.4
-- Everything works except for partail application and higher order functions.

eval :: Prog -> String -> [Integer] -> Integer
eval (Prog func) str ints = evalFuncList (Prog func) func str ints

-- Chooses to function to be excecuted based on the 3rd String parameter.
-- If nothing matches just return with -9999999, it should throw an error or search for partial application
-- But we did not complete those exercises
evalFuncList :: Prog -> [Func] ->String -> [Integer] -> Integer
evalFuncList prog [] str ints = -9999999
evalFuncList prog (x:xs) str ints = case match of 
                                    True -> evalFunction prog x ints
                                    False -> evalFuncList prog xs str ints
                        where match = evalMatchFunc x str ints

-- This evaluates a function and matches the input integers to the variables of the function head.
-- These variables are stored in a map for easy and fast lookup
-- match2 is the function responsible for creating this assiciation.
-- (match2 [(Id "")] [0]) is just a dummy variable so we can reuse the evalExpr 
-- Since there is no evalExpr that works without a map so we just create one that does not have anything
evalFunction :: Prog -> Func -> [Integer] -> Integer
evalFunction prog (Func _ expr) _ = evalExpr prog (match2 [(Id "")] [0]) expr
evalFunction prog (Function _ [] expr) _ = evalExpr prog (match2 [(Id "")] [0]) expr
evalFunction prog (Function _ list expr) ints = evalExpr prog vars expr
                        where vars = match2 list ints

evalExpr:: Prog ->Map String Integer -> Expr -> Integer
evalExpr prog vars (Term term) = evalTerm prog vars term
evalExpr prog vars (AddAndSub term str expr) = f lhs rhs
                        where f = case str of
                                    "+"-> (\ x y -> x + y)
                                    "-"-> (\ x y -> x - y)
                              lhs = evalTerm prog vars term
                              rhs = evalExpr prog vars expr

--Evaluates a list of exprs
evalExprList:: Prog ->Map String Integer -> [Expr] -> [Integer]
evalExprList _ _ [] = []
evalExprList prog vars (x:xs) = (evalExpr prog vars x) : (evalExprList prog vars xs)

evalTerm :: Prog ->Map String Integer -> Term -> Integer
evalTerm prog vars (Factor fact) = evalFactor prog vars fact
evalTerm prog vars (Mult fact term) = (evalFactor prog vars fact) * (evalTerm prog vars term)

-- Eval left, right and then compare based on the pattern match
evalOrder :: Prog ->Map String Integer ->Order-> Expr ->Expr -> Bool
evalOrder prog vars (Greater id) e1 e2 = (evalExpr prog vars e1)>(evalExpr prog vars e2)
evalOrder prog vars (Equals id)  e1 e2  = (evalExpr prog vars e1)==(evalExpr prog vars e2)
evalOrder prog vars (Less id) e1 e2  = (evalExpr prog vars e1)<(evalExpr prog vars e2)

-- Here if we see a function call we use eval again, the new parameters are in params or in [(evalExpr prog vars expr)]
-- At Identifier3 we return the value of the variable stored in the map
evalFactor :: Prog ->Map String Integer -> Factor -> Integer
evalFactor prog vars (Int i) = i
evalFactor prog vars (Parens expr) = evalExpr prog vars expr
evalFactor prog vars (Identifier3 id) = getVar vars id -- This is just an easy lookup of the variable
evalFactor prog vars (Identifier2 id expr) = eval prog id [(evalExpr prog vars expr)]
evalFactor prog vars (Identifier id expr list) = eval prog id params 
                                                where params = (evalExpr prog vars expr) : evalExprList prog vars list 
evalFactor prog vars (If e1 ord e2 e3 e4) = case ifResult of 
                                              True -> evalExpr prog vars e3
                                              otherwise -> evalExpr prog vars e4
                                        where ifResult = evalOrder prog vars ord e1 e2
                            
-- This fucntion compares a function with a String and [Integer] to see if they match
-- If they do return True to indicate this is the fucntion we need to evaluate                                       
evalMatchFunc :: Func -> String ->[Integer]-> Bool
evalMatchFunc (Func str1 expr) str2 _ = str1 == str2 
evalMatchFunc (Function str1 list _ ) str2 ints = str1 == str2 && patternMatch list ints

-- FP5.2
-- We only check if the Integers match and the length
patternMatch :: [Comb]-> [Integer]->Bool
patternMatch [] []   = True
patternMatch [] ints = False
patternMatch head [] = False 
patternMatch  ((Id x) : xs)       (head:tail) = patternMatch xs tail
patternMatch  ((Integer x) : xs)  (head:tail) = x == head && patternMatch xs tail

-- match:: [Comb]->[Integer]->Map String Integer
-- match a b = Map.fromList (zip (getStrings a) b)

-- Creates a map to store the assosciations between variables and their values
match2:: [Comb]->[Integer]->Map String Integer
match2 a b = Map.fromList (customZip a b)

getStrings:: [Comb]->[String]
getStrings [] = []
getStrings  ((Id x) : xs) = x : getStrings xs
getStrings  ((Integer x) : []) = []
getStrings  ((Integer x) : xs) = getStrings xs

-- We discard the Integers in the funciton head since we only need them for the pattern matching
-- Which is already done
customZip :: [Comb]->[Integer]-> [(String, Integer)]
customZip [] [] = []
customZip ((Id x) : xs)      (head:tail) = (x, head) : customZip xs tail
customZip ((Integer x) : xs) (head:tail) = customZip xs tail

-- Returns the value of a variable stored in the map
getVar :: Map String Integer -> String -> Integer
getVar vars str = case maybeInt of
                        Just i -> i
                        Nothing -> 0 -- ERRRORRR
                        where maybeInt = Map.lookup str vars
                                         

-- ======================== runFile ==========================

-- FP4.3
runFile :: FilePath -> [Integer] -> IO Integer
runFile x [] = runSingle [] <$> (readFile x) -- If last function has no params
runFile x y = runMatched y <$> (readFile x) -- If last function has params

runSingle :: [Integer] -> String -> Integer -- Gets the last Func of the compiled textfile, extracts the name and puts it back into a Prog, to be used by eval
runSingle y x = eval (Prog [single]) name y
          where Prog compiled = compile x
                single = getLast compiled
                name = n where Func n rest = single
                
runMatched :: [Integer] -> String -> Integer -- Does the same as runSingle, but now takes the last Func, and the Funcs above with the same name to make it work for pattern matching
runMatched y x = eval (Prog matched) name y
          where Prog compiled = compile x
                matched = getMatched compiled name 
                name = n where Function n vars rest = getLast compiled

getLast :: [Func] -> Func -- Gets the last element of a list
getLast (x:[]) = x
getLast (x:xs) = getLast xs

getMatched :: [Func] -> String -> [Func] -- Gets all the elements in the list with the same name
getMatched (x:[]) y = [x]
getMatched ((Func a b):xs) y = getMatched xs y
getMatched (x:xs) y | y == c = [x] ++ (getMatched xs y) 
                    | otherwise = getMatched xs y where Function c vars rest = x






-- ======================== Tests MicroFP.hs ===============================



-- Parser tests --
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


-- Eval tests --
prog = Prog [Function "div" [Id "x",Id "y"] (Term (Factor (If (Term (Factor (Identifier3 "x"))) (Less "<") (Term (Factor (Identifier3 "y"))) (Term (Factor (Int 0))) (AddAndSub (Factor (Int 1)) "+" (Term (Factor (Identifier "div" (Term (Factor (Parens (AddAndSub (Factor (Identifier3 "x")) "-" (Term (Factor (Identifier3 "y"))))))) [Term (Factor (Identifier3 "y"))])))))))]

smallProg1 = compile "test n := n * n;"
smallProg1Test = eval smallProg1 "test" [2] -- Correct solution is : 4

smallProg2 = compile "test a b := a * b - ( a + b ) * b;"
smallProg2Test = eval smallProg2 "test" [2,4] -- Correct solution is : -16

smallProg3 = compile "test a b := if (a * b < 20) then {0} else {1};"
smallProg3Test1 = eval smallProg3 "test" [2,4] -- Correct solution is : 0
smallProg3Test2 = eval smallProg3 "test" [5,20] -- Correct solution is : 1

smallProg4 = compile "test a b := if (a * b < 20) then {a*3+b} else {add(a, b)}; add a b := a + b;"
smallProg4Test =  eval smallProg4 "test" [2,4] -- Correct solution is 6

smallProg5 = compile "test a b := if (a * b < 20) then {a*3+b} else {add(a, b)}; add a b := a * b + a + b;"
smallProg5Test1 =  eval smallProg5 "test" [2,4] -- Correct solution is 10
smallProg5Test2 =  eval smallProg5 "add" [2,4] -- Correct solution is 14
smallProg5Test3 =  eval smallProg5 "test" [2,420] -- Correct solution is 1262

divTest = compile "div x y := if (x < y) then { 0 } else {  1 + div ((x-y), y) };"
divRun1 = eval divTest "div" [2, 10] -- Correct solution is 0
divRun2 = eval divTest "div" [100, 3] -- Correct solution is 33 

fibTest = compile "fib n := if (n < 3) then { 1 } else { fib (n-1) + fib (n-2) };"
fibRun1 = eval fibTest "fib" [10] -- Correct solution is ?

fibonacciTest = compile "fibonacci 0 := 0; fibonacci 1 := 1; fibonacci n := fibonacci (n-1) + fibonacci (n-2);"
fibonacciRun1 = eval fibonacciTest "fibonacci" [0]
fibonacciRun2 = eval fibonacciTest "fibonacci" [1]
fibonacciRun3 = eval fibonacciTest "fibonacci" [10]

ifProgGT = If (Term (Factor (Int 2))) (Less "<") (Term (Factor (Int 3))) (Term (Factor (Int 1))) (Term (Factor (Int 3)))
ifProgEQ = If (Term (Factor (Int 2))) (Equals "==") (Term (Factor (Int 2))) (Term (Factor (Int 1))) (Term (Factor (Int 3)))
ifProgCompl = If (Term (Factor (Int 2))) (Equals "==") (Term (Factor (Int 2))) (AddAndSub (Factor (Int 1)) "+" (Term (Mult (Int 2) (Factor (Int 4))))) (Term (Factor (Int 3)))
evalIfTest1 = evalFactor (Prog []) (match2 [(Id "")] [0]) ifProgGT -- Correct solution is : 3
evalIfTest2 = evalFactor (Prog []) (match2 [(Id "")] [0]) ifProgEQ -- Correct solution is : 1
evalIfTest3 = evalFactor (Prog []) (match2 [(Id "")] [0]) ifProgCompl -- Correct solution is : 9

idProg1 = Identifier3 "apple"
idProg2 = If (Term (Factor (Int 2))) (Equals "==") (Term (Factor (Int 2))) (AddAndSub (Factor (Int 1)) "+" (AddAndSub (Mult (Identifier3 "apple") (Factor (Int 3))) "-" (Term (Factor (Identifier3 "apple"))))) (Term (Factor (Int 3)))
idTest1 = evalFactor (Prog []) (match2 [(Id "apple")] [69]) idProg1 -- Correct solution is : 69
idTest2 = evalFactor (Prog []) (match2 [(Id "apple")] [69]) idProg2 -- Correct solution is : 139

partialF = runParser program (Stream "add x y := x + y; inc := add (1);")

runFilePMFib = runFile "func.txt" [10]