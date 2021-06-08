-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Tim Koree (s2341182)
-- Student 2: Bálint Takáts (s2724448) 

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb


-- ======================== Basic Parsers ==========================

-- FP2.1
-- We create a parser like this here: (char 'a') <|>(char 'b') <|> ... <|>(char 'Z')
letter :: Parser Char
letter = foldl (\x y ->  x <|> char y) (char 'a') (['b'..'z']++['A'..'Z'])

dig :: Parser Char
dig = foldl (\x y -> x <|> char y) (char '0') ['1'..'9']


-- FP2.2

between :: Parser a -> Parser b -> Parser c -> Parser b 
between a b c = (a *> b) <* c

whitespace :: Parser a -> Parser a 
whitespace a = between (manyParsers skip <|> nothing) a (manyParsers skip <|> nothing)

skip :: Parser Char
skip = (char ' ') <|> (char '\t') <|> (char '\n') <|> (char '\r') <|> (char '\v') <|> (char '\f')

nothing :: Parser String
nothing = P (\x -> [("",x)])


-- FP2.3

sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = manyParsers (p <* s <|> p)

sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> P (\x -> [([],x)])

option :: a -> Parser a -> Parser a
option x y = y <|> pure x


-- FP2.4
-- We concat a char parsers from the characters of the String
string :: String -> Parser String
string (x:[]) = (\x->x:[]) <$> char x
string (head:tail) = (\x y-> x:y) <$> char head <*> string tail

identifier:: Parser String
identifier = whitespace (manyParsers letter) 

-- We first parse the number and then we map the string with a lambda to an Integer
integer :: Parser Integer
integer = whitespace ((\x -> (read x :: Integer)) <$> p) where 
    p = ((\x y -> x:y) <$> dig <*> (manyParsers dig)) <|> (\x -> x:[]) <$> dig

symbol :: String -> Parser String 
symbol s = whitespace (string s)

parens :: Parser a -> Parser a 
parens p = between (symbol "(") p (symbol ")")

braces :: Parser a -> Parser a 
braces p = between (symbol "{") p (symbol "}")

-- This recursively parses p and then concats the output with the lambda function
manyParsers :: Parser a -> Parser [a]
manyParsers p = ((\x y -> x:y) <$> p <*> manyParsers p) <|> (\x -> x:[]) <$> p 


-- ======================== Tests ==========================

testString = runParser (string "hello") (Stream "hello babes")

indentifierTest  = runParser identifier (Stream "   id    ")

testSep1 = runParser (sep (char 'a') (char '.')) (Stream "a,a,a,a")
testSep2 = runParser (sep (char 'a') (char ',')) (Stream "a,a,a,a")
testSep3 = runParser (sep (char 'b') (char '.')) (Stream "a,a,a,a")
testSep4 = runParser (sep (char 'b') (char '.')) (Stream "b,a,a,a")

testSep11 = runParser (sep1 (char 'a') (char '.')) (Stream "a,a,a,a")
testSep12 = runParser (sep1 (char 'a') (char ',')) (Stream "a,b,a,a")
testSep13 = runParser (sep1 (char 'b') (char '.')) (Stream "a,a,a,a")
testSep14 = runParser (sep1 (char 'b') (char '.')) (Stream "a,a,a,a")
testEmpty = runParser (char 'a') (Stream [])

testWhiteSpace = runParser (whitespace (string "apple")) (Stream "   apple  ddf ")
testWhiteSpace1 = runParser (whitespace (string "alma")) (Stream "   alma   ")

testWhiteSpace2 = runParser (whitespace (string "alma")) (Stream "alma  ddf ")
testWhiteSpace3 = runParser (whitespace (string "alma")) (Stream "   alma")

testBetween = runParser (between (char 'a') (char 'b') (char 'a')) (Stream ['a','b','a'])
testBetween1 = runParser (between (char 'a') (char 'b') (char 'a')) (Stream ['a','b','v'])

testLetterParser1 = runParser letter (Stream "abc")
testLetterParser2 = runParser letter (Stream ['1','b','c'])

testDigitParser1  = runParser dig (Stream ['a','b','c'])
testDigitParser2  = runParser dig (Stream ['1','b','c'])