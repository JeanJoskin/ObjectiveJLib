module Language.JsLib.Parser.Prim where

import Text.ParserCombinators.Parsec hiding (many,(<|>))
import Text.ParserCombinators.Parsec.Pos
import Language.JsLib.Scanner.Tokens
import Language.JsLib.AST
import Control.Applicative

type JsParser a = GenParser Token UserState a

data UserState = UserState {  permissiveSemicolon :: Bool }

defaultState :: UserState
defaultState = UserState { permissiveSemicolon = True }

-------------------------------------------------------------------------------
-- Primary parsers
-------------------------------------------------------------------------------

-- |Parses a keyword token
pReserved :: String -> JsParser String
pReserved v = token show position m
  where
    m (Reserved v' _) | v == v' = Just v
    m _               = Nothing

-- |Parses a value token of a given type
pValToken :: ValTokenType -> JsParser String
pValToken ty = token show position m
  where
    m (ValToken ty' v _) | ty == ty' = Just v
    m _                  = Nothing

-- |Parses a value token of a given type and value
pReservedVal :: ValTokenType -> String -> JsParser String
pReservedVal ty v = token show position m
  where
    m (ValToken ty' v' _) | ty == ty' && v == v' = Just v
    m _                   = Nothing

-------------------------------------------------------------------------------
-- Utility parsers
-------------------------------------------------------------------------------

-- |Parses an identifier
pIdent :: JsParser String
pIdent = pValToken TkIdent

pCommaList :: JsParser a -> JsParser [a]
pCommaList p = sepBy p pComma

pChoice :: [GenParser tok st a] -> GenParser tok st a
pChoice = foldr (<|>) pzero

pOp :: (a,String) -> JsParser a
pOp (sem,op) = sem <$ pReserved op

anyOp :: [(a,String)] -> JsParser a
anyOp = pChoice . map pOp

pPack :: String -> JsParser a -> String -> JsParser a
pPack o p c = pReserved o *> p <* pReserved c

(<??>) :: GenParser tok st a -> GenParser tok st (a -> a) -> GenParser tok st a
p <??> q = p <**> option id q

pMaybe :: GenParser tok st a -> GenParser tok st (Maybe a)
pMaybe = optionMaybe

pComma :: JsParser String
pComma = pReserved ","

pSemi :: JsParser String
pSemi = do
          s <- getState
          if (permissiveSemicolon s)
            then option ";" (pReserved ";")
            else pReserved ";"

readNumeric :: String -> Double
readNumeric s | head s == '.' = read ('0':s)
              | otherwise     = read s
