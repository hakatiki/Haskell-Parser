{-# LANGUAGE PackageImports #-} 
module CheckZip where

-- Installation:
-- Ubuntu / Linux: install libbz2-dev
-- Install Haskell Packages using Cabal:
-- - zip-1.2.0
-- - regex-tdfa
-- - bytestring
--
-- Usage:
-- 1) [optional] add your student numbers and names in the definitions of 'student1' and 'student2' below
-- 2) Either:
-- 2a) Command line: runghc CheckZip
--  -or-
-- 2b) GHCi: main
-- 3) Supply your names and student numbers if you skipped step 1


import "zip" Codec.Archive.Zip
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

data Student = Student { stname :: String, stnum :: String} deriving (Eq, Ord)



------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- This information should also be in PComb.hs, BasicParsers.hs and MicroFP.hs
student1 = Student { stname = "", stnum = "" } -- Update
student2 = Student { stname = "", stnum = "" } -- Keep empty if you submit alone
------------------------------------------------------------------------------
------------------------------------------------------------------------------








instance Show Student where
  show (Student name num) = "Student {name='" ++ name ++ "', stnum='" ++ num ++ "'}"

trimInitial :: String -> String
trimInitial = dropWhile isSpace

trim :: String -> String
trim = trimInitial . reverse . trimInitial . reverse


-- Find student 1 or 2 in the specified file
findstudent :: Int -> String -> Maybe Student
findstudent id  xs | null s     = Nothing 
                   | otherwise  = Just (Student (trim $ head s) (trim $ last s))
  where (_,_,_,s) = (xs =~ ("-- Student "++ show id++": ([^(]+)\\((s[0-9xy]+)\\)")) :: (String, String, String, [String])


-- Get the students in the file 'fname' in the zip-file 'zipname'
checkFileInZip :: FilePath -> String -> IO (Student, Student)
checkFileInZip zipname fname  = do f <- mkEntrySelector fname
                                   contents <- (B.unpack) <$> withArchive zipname (getEntry f)
                                   let s1 = findstudent 1 contents
                                   let s2 = findstudent 2 contents
                                   when (isNothing s1) $
                                     fail ("`-- Student 1' not specified in " ++ fname)
                                   when (isNothing s2) $
                                     fail ("`-- Student 2' not specified in " ++ fname)
                                   when (stnum (fromJust s1) == "sxxxxxxx" ||  stname (fromJust s1)  == "Your Name") $
                                     fail $ "Please add your names and student numbers to " ++ fname

                                   return (fromJust s1, fromJust s2)

-- Check the zip files for the two given students
checkZip s1 s2 = do putStrLn $ "Checking zip file '" ++ zipname ++ "'"
                    hasFile pcomb
                    hasFile basicparser
                    hasFile microfp
                    checkContent pcomb
                    checkContent basicparser
                    checkContent microfp
                    
 where dirname = if stnum s2=="" then (stnum s1) else (stnum s1) ++ "_" ++ (stnum s2)
       zipname = dirname ++ ".zip"
       pcomb = dirname ++ "/PComb.hs"
       microfp = dirname ++ "/MicroFP.hs"
       basicparser = dirname ++ "/BasicParsers.hs"
       hasFile f = do f' <- mkEntrySelector f
                      dir <- withArchive zipname (doesEntryExist f')
                      unless dir
                        $ fail $ "The zip archive '" ++ zipname ++ "' does not contain the file '" ++ f ++ "' (does the zip-file contain the directory '" ++ dirname ++ "'?)"
       checkContent f = do (s1',s2') <- checkFileInZip zipname f
                           if (s1 /= s1') then (fail ("Student in `" ++ f ++ "' is [" ++ show s1' ++ "], different from specified student: " ++ show s1))
                             else pure ()

-- Read a student number and check if it is correct
getsnum = do snum <- getLine
             if snum =~ "^s[0-9]{7,7}$" || null snum then
               return snum
             else do
               putStrLn "Invalid student number: a student number starts with 's' and is followed by 7 digits"
               putStrLn "Student number?"
               getsnum

-- Read a student name and number, if 'student1' is not a valid student
getStudent :: IO (Student,Student)
getStudent = do if (stname student1 /= "") then
                  do unless (stnum student1 =~ "^s[0-9]{7,7}$") $ fail $ "The student number '" ++ stnum student1 ++ "' is invalid; a student number starts with 's' and is followed by 7 digits"
                     unless (stnum student2 =~ "^s[0-9]{7,7}$") $ fail $ "The student number '" ++ stnum student2 ++ "' is invalid; a student number starts with 's' and is followed by 7 digits"
                     return (student1, student2)
                else do
                  putStrLn "\n\n[If you want to skip these questions, modify 'student1' and (when needed) 'student2' in 'CheckZip.hs']\n\n"

                  putStrLn "Name of student 1 (case sensitive, as specified in PComb.hs)?"
                  name1 <- getLine
                  putStrLn $ "\nStudent number (starting with s) of " ++ name1
                  snum1 <- getsnum

                  putStrLn "\n\n[If you work alone leave the don't supply a student 2 (press enter twice)]"

                  putStrLn "\nName of student 2 (case sensitive, as specified in PComb.hs)?"
                  name2 <- getLine
                  putStrLn $ "\nStudent number (starting with s) of " ++ name2
                  snum2 <- getsnum

                  return (Student name1 snum1, Student name2 snum2)
             
main = do (s1,s2) <- getStudent
          checkZip s1 s2
          putStrLn "\nYour zip-file passed all basic checks! :-)"
          putStrLn "This means: the zip-file can be opened, contains the right files and your names/student numbers are in the file"
          putStrLn "Before submitting, please check if your code follows all other requirements!"
          return ()
          
