--
-- Copyright (c) 2011, Jean Joskin
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of Jean Joskin nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL JEAN JOSKIN BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module Language.JsLib.Printer.Util where

import Text.PrettyPrint.HughesPJ
import Data.List

sepList :: Doc -> [Doc] -> Doc
sepList sep xs = foldr (<>) empty (intersperse sep xs) 

commaList = sepList (text ", ")

vSepList :: Doc -> [Doc] -> Doc
vSepList sep [] = empty
vSepList sep xs = foldr1 (\x r -> x <> sep $$ r) xs

maybeText :: Maybe String -> Doc
maybeText (Just s) = text s
maybeText Nothing  = empty

blockBraces :: Doc -> Doc
blockBraces d | isEmpty d = lbrace <+> rbrace
              | otherwise = lbrace $+$ (nest 4 d) $+$ rbrace

objectBraces :: Doc -> Doc
objectBraces d | isEmpty d = lbrace <+> rbrace
               | otherwise = lbrace $+$ (nest 2 d) $+$ rbrace

indentStmt :: Bool -> Doc -> Doc
indentStmt isBlock d | isBlock = d
                     | otherwise = nest 4 d

maybeParens :: Doc -> Doc
maybeParens d | isEmpty d = empty
              | otherwise = parens d

-- | Pretty print a number
prettyNum :: Double -> Doc
prettyNum n | n - (fromIntegral flr) == 0 = text (show flr)
            | otherwise = text (show n)
  where
    flr = (floor n) :: Int
