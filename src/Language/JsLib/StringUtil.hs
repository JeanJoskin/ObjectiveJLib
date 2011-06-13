module Language.JsLib.StringUtil
( readNumeric
, readQuotedString
, writeQuotedString
) where

import Data.List
import Data.Char

-- NOTE
-- These functions are written under assumption that their inputs
-- are correctly formatted. We can assume this, since everything is
-- checked by our scanner first.

hexChars = "0123456789ABCDEF"

readNumeric :: String -> Double
readNumeric s | head s == '.' = read ('0':s)
              | otherwise     = read s

escapeChars = [ ('b','\b'), ('f','\f'), ('n','\n'), ('r','\r'), ('t','\t'), ('v','\v') ]

readQuotedString :: String -> String
readQuotedString (x:xs) = readString (init xs)

writeQuotedString :: String -> String
writeQuotedString xs = '"':writeString '"' xs ++ "\""

readString :: String -> String
readString [] = []
readString ('\\':xs) = case xs of
                          'x':ys -> let (hexStr,r) = splitAt 2 ys
                                    in  chr (unhex hexStr) : readString r
                          'u':ys -> let (unicodeStr,r) = splitAt 4 ys
                                    in  chr (unhex unicodeStr) : readString r
                          c:ys   -> maybe c id (lookup c escapeChars) : readString ys
readString (x:xs) = x : readString xs

writeString :: Char -> String -> String
writeString q [] = []
writeString q (x:xs) | x == q                 = '\\':q:writeString q xs
                     | isAscii x && isPrint x = x : writeString q xs
                     | isLatin1 x             = ('\\':'x':pad '0' 2 (hex x')) ++ writeString q xs
                     | otherwise              = ('\\':'u':pad '0' 4 (hex x')) ++ writeString q xs
  where
    x' = ord x

hex :: Int -> String
hex 0 = "0"
hex i = reverse (hex' i)
  where
    hex' 0 = ""
    hex' i = hexChars !! (i `mod` 16) : hex' (i `div` 16)

unhex :: String -> Int
unhex = unhex' 0
  where
    unhex' t [] = t
    unhex' t (x:xs) = case (findIndex ((==) (toUpper x)) hexChars) of
                        (Just i) -> unhex' (i + (t * 16)) xs
                        _        -> error "unhex: invalid hex character"

pad :: Char -> Int -> String -> String
pad c len s | len > length s = f (len - length s) s
            | otherwise      = s
  where
    f 0 s = s
    f n s = c : f (n-1) s
