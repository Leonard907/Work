-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 4(07-11 Oct.)

module Tutoria4 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL :: String
testURL = "http://www.mit.edu/contact/"

url1 :: String
url1 = "http://www.sps.ed.ac.uk/gradschool/contact"

testHTML :: String
testHTML = "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]

testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Irene Vlassi","irene.vp@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

-- </system interaction>
-- <exercises>

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)


-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = map toLower str1 == map toLower str2


-- 2.
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix sub str = or [sameString (map toLower sub) $ take n (map toLower str) | n <- [0..length sub]]

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
                         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
                
-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains str substr = or [prefix substr $ drop n str | n <- [0..length str]]
                         

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = (map toUpper str) `contains` substr &&
                        (map toLower str) `contains` substr
                              where
                                substr = take n (drop m str)   

-- 4.
takeUntil :: String -> String -> String
takeUntil substr str
    | contains str substr = [str !! i | i <- [0..frontLen - 1]]
    | otherwise = str
    where
        frontLen = head [n | n <- [0..length str], prefix substr $ drop n str]
        

dropUntil :: String -> String -> String
dropUntil substr str
    | contains str substr = drop ((length $ takeUntil substr str) + (length substr)) str
    | otherwise = []

-- 5.

split :: String -> String -> [String]
split [] _ = error "separator is empty"
split _ [] = []
split sep line = takeUntil normalsep line : split normalsep (dropUntil normalsep line)
                 where normalsep = map toLower sep

reconstruct :: String -> [String] -> String
reconstruct _ [] = []
reconstruct _ [s] = s
reconstruct sep strs = foldr1 (\x y -> x ++ sep ++ y) strs

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML = split "<a href=\""

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks

-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [e | e <- links, prefix "mailto:" e]

-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (dropUntil "\">" purelink,takeUntil "\">" purelink)
                 where
                   purelink = takeUntil "</a" $ dropUntil "mailto:" link

-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = [link2pair h | h <- takeEmails $ linksFromHTML html]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook

-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name addressBook = [(fname,email) | (fname,email) <- addressBook, contains fname name ]
                                

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name $ emailsFromHTML html

-- Optional Material

-- 13.
hasInitials :: String -> Name -> Bool
hasInitials initials fname = sameString initials capitals
                               where
                                 capitals = [head p | p <- split " " fname]

-- 14.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f html = [(fname,email) | (fname,email) <- emailsFromHTML html, f fname]
    
emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML initials html = [(name, email) | (name,email) <- emailsFromHTML html, hasInitials initials name]

-- 15.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria name = sameString "PW" [head $ head parts , head $ last parts]
                   where
                     parts = split " " name

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML html = [(name, email) | (name,email) <- emailsFromHTML html, myCriteria name]

-- 16.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ (sep name) ++ concat (replicate (20 - (length $ sep name)) " ") ++ (if sameString name email then [] else email) | (name,email) <- addr ]
            where
              sep name
                  | length (splits name) == 1 = last $ splits name
                  | otherwise = (last $ splits name) ++ ", " ++ (head $ splits name)
              splits name = split " " name
              