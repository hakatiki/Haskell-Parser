-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Tim Koree (s2341182)
-- Student 2: Bálint Takáts (s2724448) 

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- ======================== 1st series ========================

-- FP1.1
data Parser a = P { 
    runParser :: Stream -> [(a,Stream)]
    } 
-- FP1.2
instance Functor Parser where
    fmap f p = P (\x -> [ (f r, s) | (r,s) <- runParser p x ])

-- FP1.3

char :: Char -> Parser Char
char c = P p
    where p (Stream stream) | stream == [] = []
                            | c == x = [(c, Stream xs)]
                            | otherwise = []
                where (x:xs) = stream
-- FP1.4
failure :: Parser a
failure = P (\cs -> [])  

-- FP1.5
instance Applicative Parser where
    pure v = P (\s -> [(v,s)]) 
    p1 <*> p2 = P (\s -> [ (r1 r2, s2)
                        | (r1, s1) <- runParser p1 s,
                          (r2, s2) <- runParser p2 s1 ])

-- FP1.6                      
instance Alternative Parser where
    empty =  P (\cs -> [])
    p1 <|> p2 = P (\x -> case (runParser p1 x) of 
                [] -> (runParser p2 x) 
                s -> s)


-- ======================== 1st series test ========================

testCharParser = runParser (char 'a') (Stream ['a','b','c'])



-- ======================== 2nd series ========================
-- FP2.1
letter :: Parser Char
letter = foldl (\x y ->  x <|> char y) (char 'a') (['b'..'z']++['A'..'Z'])

dig :: Parser Char
dig = foldl (\x y -> x <|> char y) (char '0') ['1'..'9']


-- FP2.2

between :: Parser a -> Parser b -> Parser c -> Parser b 
between a b c = (\ x y z -> y) <$> a<*>b<*>c

skip :: Parser Char
skip = (char ' ') <|> (char '\t') <|> (char '\n') <|> (char '\r') <|> (char '\v') <|> (char '\f')

nothing :: Parser String
nothing = P (\x -> [("",x)])

whitespace :: Parser a -> Parser a 
whitespace a = between (manyParsers skip <|> nothing) a (manyParsers skip <|> nothing)


-- FP2.3


sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> P (\x -> [([],x)])

sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = manyParsers (p <* s <|> p)


-- FP2.4

string :: String -> Parser String
string (x:[]) = (\x->x:[]) <$> char x
string (head:tail) = (\x y-> x:y) <$> char head <*> string tail

option :: a -> Parser a -> Parser a
option x y = y <|> pure x

integer :: Parser Integer
integer = whitespace ((\x -> (read x :: Integer)) <$> p) where 
    p = ((\x y -> x:y) <$> dig <*> (manyParsers dig)) <|> (\x -> x:[]) <$> dig

symbol :: String -> Parser String 
symbol s = whitespace (string s)

manyParsers :: Parser a -> Parser [a]
manyParsers p = ((\x y -> x:y) <$> p <*> manyParsers p) <|> (\x -> x:[]) <$> p 

identifier:: Parser String
identifier = whitespace (manyParsers letter) 

braces :: Parser a -> Parser a 
braces p = between (symbol "{") p (symbol "}")

parens :: Parser a -> Parser a 
parens p = between (symbol "(") p (symbol ")")



-- ===== testParsers =====
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