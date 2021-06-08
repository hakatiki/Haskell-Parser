-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Tim Koree (s2341182)
-- Student 2: Bálint Takáts (s2724448) 

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck


-- ======================== Stream ==========================

data Stream = Stream [Char]
              deriving (Eq, Show)


-- ======================== Parser Combinator Library ========================


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



-- ======================== Tests ==========================

testCharParser = runParser (char 'a') (Stream ['a','b','c'])