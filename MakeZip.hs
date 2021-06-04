{-# LANGUAGE PackageImports #-}
module MakeZip where

-- You may use this file to generate a zip-file that follows the submission criteria.
-- No support is given for using this tool -  if it does not work (correctly), please do not spend too much time on getting it to work and create the zip-file by hand following the criteria.

-- Make sure the "-- Student 1" and "-- Student 2" lines are in each file, and only change the names and student numbers.
-- When you work alone, please do not change the "-- Student 2" line.

-- Installation:
-- Ubuntu / Linux: install libbz2-dev
-- Install Haskell Packages using Cabal:
-- - zip-1.2.0
-- - regex-tdfa
-- - bytestring


import "zip" Codec.Archive.Zip
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

trimInitial :: String -> String
trimInitial = dropWhile isSpace

trim :: String -> String
trim = trimInitial . reverse . trimInitial . reverse

getStudent :: String -> IO String
getStudent s = do putStrLn $ "Student number of student " ++ s
                  m <- getLine
                  if m =~ "^s[0-9]{7,7}$" || null m then
                    return m
                  else
                    getStudent s

addFile :: String -> String -> ZipArchive ()
addFile prefix file = do entry <- mkEntrySelector $ prefix ++ "/" ++ file
                         loadEntry Store entry file

makeArchive :: String -> ZipArchive ()
makeArchive prefix = do addFile prefix "PComb.hs"
                        addFile prefix "BasicParsers.hs"
                        addFile prefix "MicroFP.hs"
                        addFile prefix "functions.txt"

data Student = Student { stname :: String, stnum :: String} deriving Eq

-- Find student 1 or 2 in the specified file
findstudent :: Int -> String -> Maybe Student
findstudent id  xs | null s     = Nothing 
                   | otherwise  = Just (Student (trim $ head s) (trim $ last s))
  where (_,_,_,s) = (xs =~ ("-- Student "++ show id++": ([^(]+)\\((s[0-9xy]+)\\)")) :: (String, String, String, [String])


checkFile :: FilePath -> IO (Student, Student)
checkFile fname  = do contents <- readFile fname
                      let s1 = findstudent 1 contents
                      let s2 = findstudent 2 contents
                      when (isNothing s1) $ fail $ "`-- Student 1' not specified in " ++ fname
                      when (isNothing s2) $ fail $ "`-- Student 2' not specified in " ++ fname
                      when (stnum (fromJust s1) == "sxxxxxxx" ||  stname (fromJust s1)  == "Your Name") $
                        fail $ "Please add your name and student numbers to " ++ fname

                      return (fromJust s1, fromJust s2)
                     
verifyFile :: FilePath -> (Student, Student) -> IO ()
verifyFile fname (s1,s2) = do (s1',s2') <- checkFile fname
                              when (s1 /= s1') $ fail $ "Incorrect student 1 in " ++ fname ++ ": it should match PComb.hs"
                              when (stnum s2 /= "syyyyyyy" && s2 /= s2') $ fail $ "Incorrect student 2 in " ++ fname ++ ": it should match PComb.hs"

prompt :: String -> IO Char
prompt s = do putStrLn $ s ++ " [Y/N]"
              r <- getChar
              if (r `elem` "YN") then (return r) else (prompt s)

main :: IO ()
main = do (s1, s2) <- checkFile "PComb.hs"
          a1 <- prompt $ "[first student] Is  the student number of " ++ stname s1 ++ "`" ++ stnum s1 ++ "'?"
          when (a1 /= 'Y') $ fail "Please update the student number in PComb.hs"
          let hass2 = stnum s2 /= "syyyyyyy" -- Is there a second student?
          putStrLn $ stnum s2
          if hass2 then
            do a2 <- prompt $ "[second student] Is the student number of " ++ stname s2 ++ "`" ++ stnum s2 ++ "'?"
               when (a2 /= 'Y') $ fail "Please update the student number in PComb.hs"
          else
            putStrLn "No second student found in PComb.hs, assuming a group of one student"

          verifyFile "BasicParsers.hs" (s1,s2)
          verifyFile "MicroFP.hs" (s1,s2)

          let prefix = if hass2 then stnum s1 ++ "_" ++ stnum s2 else stnum s1
          putStrLn $ "Creating the archive: " ++ prefix ++ ".zip"
          createArchive (prefix ++ ".zip") (makeArchive prefix)
          putStrLn "Archive created! Please check if everything is correct before submitting."
