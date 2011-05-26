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

module Language.JsLib.Parser.Parser (parse) where

import Text.ParserCombinators.Parsec hiding (parse,many,(<|>))
import Text.ParserCombinators.Parsec.Pos
import Language.JsLib.Scanner.Tokens
import Language.JsLib.AST
import Control.Applicative
import Language.JsLib.Parser.Prim
import Data.Map (Map)
import Data.List (intersperse)
import qualified Data.Map as Map

pArguments :: JsParser [Expression]
pArguments = pPack "(" (pCommaList pAssignmentExpression) ")"

pFunctionBody :: JsParser [SourceElement]
pFunctionBody = many pSourceElement

-------------------------------------------------------------------------------
-- Expression parsing
-------------------------------------------------------------------------------

-- Literal (7.8)
pLiteral :: JsParser Expression
pLiteral = pENull <|> pEBool <|> pENumeric <|> pEString <?> "literal"

pENull = ENull <$ pReserved "null"
pEBool = EBool <$> anyOp [(True,"true"),(False,"false")]
pENumeric = ENumeric . readNumeric <$> pValToken TkNumeric
pEString = EString <$> pValToken TkString

-- PrimaryExpression (11.1) (+RegExp)
pPrimaryExpression :: JsParser Expression
pPrimaryExpression = pEThis <|> pEIdent <|> pLiteral <|> pERegExp <|>
                       pEArray <|> pEObject <|> pEExpression <|> pEJString <|>
                       pEJNil <|> pEJSuper <|> pEJSelf <|> pEJSelector
                       <?> "primary expression"

pEThis = EThis <$ pReserved "this"
pEIdent = EIdent <$> pValToken TkIdent
pERegExp = ERegExp <$> pValToken TkRegExp
pEObject = EObject <$> pPack "{" (pCommaList pPropertyAssignment) "}"
pEArray = EArray <$> pPack "[" (pCommaList pAssignmentExpression) "]"
pEExpression = EExpression <$> pPack "(" pExpression ")"

-- PropertyName (11.1.5)
-- Warning: pEJString is only allowed for compatibility with existing
--          code. It is invalid according to the ECMAScript spec.
pPropertyName :: JsParser PropertyName
pPropertyName = pPNIdent <|> pPNString <|> pPNNumeric <|> pPNCPString <?> "property name"

pPNIdent = PNIdent <$> pValToken TkIdent
pPNString = PNString <$> pValToken TkString
pPNNumeric = PNNumeric . read <$> pValToken TkNumeric
pPNCPString = PNCPString <$ pReserved "@" <*> pValToken TkString

-- PropertyAssignment (11.1.5)
pPropertyAssignment :: JsParser PropertyAssignment
pPropertyAssignment = try pPAGet <|> try pPASet <|> pPAExpr <?> "property assignment"

pPAExpr = PAExpr <$> pPropertyName <* pReserved ":" <*> pAssignmentExpression
pPAGet = PAGet <$ pReservedVal TkIdent "get" <*> pPropertyName <* pReserved "(" <*
                  pReserved ")" <* pReserved "{" <*> pFunctionBody <*
                  pReserved "}"
pPASet = PASet <$ pReservedVal TkIdent "set" <*> pPropertyName <* pReserved "(" <*>
                  pIdent <* pReserved ")" <* pReserved "{" <*> pFunctionBody <*
                  pReserved "}"

-- MemberExpression (11.2)
pMemberExpression :: JsParser Expression
pMemberExpression = pPrimaryExpression <??> pMemberExpressionPost <|>
                      pEFunction <??> pMemberExpressionPost <|>
                      pENew <??> pMemberExpressionPost

pMemberExpressionPost :: JsParser (Expression -> Expression)
pMemberExpressionPost = flip (.) <$> pEIndex <*> option id pMemberExpressionPost <|>
                          flip (.) <$> pEDot <*> option id pMemberExpressionPost

pEFunction = EFunction <$ pReserved "function" <*> pMaybe pIdent
                 <*> pPack "(" (pCommaList pIdent) ")"
                 <*> pPack "{" pFunctionBody "}" <?> "function definition"
pENew = ENew <$ pReserved "new" <*> pMemberExpression <*> option [] pArguments <?> "new"
pEIndex = flip EIndex <$> pPack "[" pExpression "]" <?> "index"
pEDot = flip EDot <$ pReserved "." <*> pIdent

-- CallExpression (11.2) (modified)
pCallExpression :: JsParser Expression
pCallExpression = pEJMessage <??> pCallExpressionPost <|>
                    pCECallMemberExpr <??> pCallExpressionPost

pCallExpressionPost :: JsParser (Expression -> Expression)
pCallExpressionPost = flip (.) <$> pCECall <*> option id pCallExpressionPost <|>
                        flip (.) <$> pCEIndex <*> option id pCallExpressionPost <|>
                        flip (.) <$> pCEDot <*> option id pCallExpressionPost

pCECallMemberExpr = pMemberExpression <??> (flip ECall <$> pArguments)

pCECall = flip ECall <$> pArguments
pCEIndex = flip EIndex <$> pPack "[" pExpression "]"
pCEDot = flip EDot <$ pReserved "." <*> pIdent

-- LeftHandSideExpression (11.2) (modified)
pLeftHandSideExpression :: JsParser Expression
pLeftHandSideExpression =  pCallExpression

-- PostFixExpression (11.3)
postfixOps = anyOp [(EPostInc,"++"),(EPostDec,"--")] <?> "postfix operator"

pPostFixExpression :: JsParser Expression
pPostFixExpression = pLeftHandSideExpression <??> postfixOps

-- UnaryExpression (11.4)
unaryOps = anyOp [ (EDelete,"delete"),(EVoid,"void"),(ETypeOf,"typeof"),
             (EPreInc,"++"),(EPreDec,"--"),(EUnaryPlus,"+"),(EUnaryMin,"-"),
             (EBitNot,"~"),(ELogicNot,"!") ] <?> "unary operator"

pUnaryExpression :: JsParser Expression
pUnaryExpression = (unaryOps <*> pUnaryExpression) <|> pPostFixExpression

-- Infix Operator Expressions (11.5 - 11.11)
infixOpList = [ (ELogicOR,"||"),(ELogicAND,"&&"),(EBitOR,"|"),(EBitXOR,"^"),
                   (EBitAND,"&"),(EEqual,"=="),(ENotEqual,"!="), (EStrictEqual,"==="),
                   (EStrictNotEqual,"!=="),(ELess,"<"), (EGreater,">"),
                   (ELessEqual,"<="),(EGreaterEqual,">="),(EInstanceof,"instanceof"),
                   (EIn,"in"),(ESignedShiftLeft,"<<"),(ESignedShiftRight,">>"),
                   (EUnsignedShiftRight,">>>"),(EAdd,"+"),(ESubtract,"-"),
                   (EModulus,"%"),(EDivide,"/"),(EMultiply,"*") ]

infixOpListNoIn = filter ((/=) "in" . snd) infixOpList

infixOps = anyOp infixOpList <?> "infix operator"
infixOpsNoIn = anyOp infixOpListNoIn <?> "infix operator (excluding in)"

pInfixOpExpression :: JsParser Expression
pInfixOpExpression = pUnaryExpression `chainl1` infixOps

-- ConditionalExpression (11.12)
pConditionalExpression :: JsParser Expression
pConditionalExpression = pInfixOpExpression <??> pEConditional

pEConditional = (\a b p -> EConditional p a b) <$ pReserved "?" <*>
                  pAssignmentExpression <* pReserved ":" <*>
                  pAssignmentExpression <?> "conditional"

-- AssignmentExpression (11.13) (modified)
-- Fixme: Lefthand-side should not be a conditional, infix, unary or postfix

assignOps = anyOp [(AEquals,"="),(AMultiply,"*="),(ADivide,"/="),(AModulus,"%="),
             (AAdd,"+="),(ASubtract,"-="),(ASignedShiftLeft,"<<="),
             (ASignedShiftRight,">>="),(AUnsignedShiftRight,">>>="),
             (ABitAND,"&="),(ABitXOR,"^="),(ABitOR,"|=")]

pAssignmentExpression :: JsParser Expression
pAssignmentExpression = pConditionalExpression <??> pEAssignment

pEAssignment = (\op rhs lhs -> EAssign lhs op rhs) <$>
                   assignOps <*> pAssignmentExpression <?> "assignment"

-- Expression (11.14)
pExpression :: JsParser Expression
pExpression = pAssignmentExpression `chainr1` pOp (EComma,",")

-------------------------------------------------------------------------------
-- Statement parsing
-------------------------------------------------------------------------------

-- Statement (12)
pStatement :: JsParser Statement
pStatement = pBlock <|> pVariableStatement <|> pEmptyStatement <|>
               pIfStatement <|> pIterationStatement <|> pContinueStatement <|>
               pContinueStatement <|> pBreakStatement <|> pReturnStatement <|>
               pWithStatement <|> pSwitchStatement <|> pLabelledStatement <|>
               pThrow <|> pTry <|> pDebugger <|> pExpressionStatement

-- Block (12.1)
pBlock :: JsParser Statement
pBlock = SBlock <$> pPack "{" (many pStatement) "}"

-- VariableStatement (12.2)
pVariableStatement :: JsParser Statement
pVariableStatement = SVariable <$ pReserved "var" <*> pCommaList pVariableDeclaration <* pSemi

-- VariableDeclaration (12.2)
pVariableDeclaration :: JsParser Decl
pVariableDeclaration = Decl <$> pIdent <*> (pMaybe pInitializer)
  where
    pInitializer = pReserved "=" *> pAssignmentExpression

-- EmptyStatement (12.3)
pEmptyStatement :: JsParser Statement
pEmptyStatement = SEmpty <$ pReserved ";"

-- ExpressionStatement (12.4)
pExpressionStatement :: JsParser Statement
pExpressionStatement =  SExpression <$ notFollowedBy (pReserved "function") <*>
                          pExpression <* pSemi

-- IfStatement (12.5)
pIfStatement :: JsParser Statement
pIfStatement = SIf <$ pReserved "if" <*> pPack "(" pExpression ")" <*> 
	             pStatement <*> pMaybe (pReserved "else" *> pStatement)

-- IterationStatement (12.6)
pIterationStatement :: JsParser Statement
pIterationStatement = pSDoWhile <|> pSWhile <|> pSFor

pSDoWhile = SDoWhile <$ pReserved "do" <*> pStatement <* pReserved "while" <*>
              pPack "(" pExpression ")" <* pSemi
pSWhile = SWhile <$ pReserved "while" <*> pPack "(" pExpression ")" <*>
              pStatement
pSFor = SFor <$ pReserved "for" <*> pPack "(" pForClause ")" <*> pStatement


pForClause :: JsParser ForClause
pForClause = try pFCExprExprExpr <|> try pFCVarExprExpr <|> try pFCLhsIn <|> pFCVarIn

pFCExprExprExpr = FCExprExprExpr <$> pMaybe pExpression <* pReserved ";" <*>
                    pMaybe pExpression <* pReserved ";" <*> pMaybe pExpression
pFCVarExprExpr = FCVarExprExpr <$ pReserved "var" <*> pCommaList pVariableDeclaration <*
                   pReserved ";" <*> pMaybe pExpression <* pReserved ";" <*>
                   pMaybe pExpression
pFCLhsIn = FCLhsIn <$> pLeftHandSideExpression <* pReserved "in" <*> pExpression
pFCVarIn = FCVarIn <$ pReserved "var" <*> pVariableDeclaration <*
             pReserved "in" <*> pExpression

-- ContinueStatement (12.7)
pContinueStatement :: JsParser Statement
pContinueStatement = SContinue <$ pReserved "continue" <*> pMaybe pIdent <* pSemi

-- BreakStatement (12.8)
pBreakStatement :: JsParser Statement
pBreakStatement = SBreak <$ pReserved "break" <*> pMaybe pIdent <* pSemi

-- ReturnStatement (12.9)
pReturnStatement :: JsParser Statement
pReturnStatement = SReturn <$ pReserved "return" <*> pMaybe pExpression <* pSemi

-- WithStatement (12.10)
pWithStatement :: JsParser Statement
pWithStatement = SWith <$ pReserved "with" <*> pPack "(" pExpression ")" <*> pStatement

-- SwitchStatement (12.11)
pSwitchStatement :: JsParser Statement
pSwitchStatement = SSwitch <$ pReserved "switch" <*> pPack "(" pExpression ")" <*>
                       pPack "{" (many pCaseClause) "}"

pCaseClause :: JsParser CaseClause
pCaseClause = pCCCase <|> pCCDefault

pCCCase = CCCase <$ pReserved "case" <*> pExpression <* pReserved ":" <*> many pStatement
pCCDefault = CCDefault <$ pReserved "default" <* pReserved ":" <*> many pStatement

-- LabelledStatement (12.12)
pLabelledStatement :: JsParser Statement
pLabelledStatement = SLabel <$ notFollowedBy pExpression <*> pIdent <* pReserved ":" <*> pStatement

-- ThrowStatement (12.13)
pThrow :: JsParser Statement
pThrow = SThrow <$ pReserved "throw" <*> pExpression <* pSemi

-- TryStatement (12.14)
pTry :: JsParser Statement
pTry = STry <$ pReserved "try" <*> pBlock <*> pMaybe pCatch <*> pMaybe pFinally

pCatch = CatchClause <$ pReserved "catch" <*> pPack "(" pIdent ")" <*> pBlock
pFinally = pReserved "finally" *> pBlock

-- DebuggerStatement (12.15)
pDebugger :: JsParser Statement
pDebugger = SDebugger <$ pReserved "debugger" <* pSemi

-------------------------------------------------------------------------------
-- Objective J
-------------------------------------------------------------------------------

pEJSelf :: JsParser Expression
pEJSelf = EJSelf <$ pReserved "self"

pEJNil :: JsParser Expression
pEJNil = EJNil <$ pReserved "nil"

pEJSuper :: JsParser Expression
pEJSuper = EJSuper <$ pReserved "super"

pJTy :: JsParser JTy
pJTy = pJPrimTy <??> pJTySuffix <|> pJExtTy
 
pJPrimTy = pJTyId <|> pJTyVoid <|> pJTyBool <|> pTySimpleSigned <|> pTySimpleUnsigned <|>
             pJTyChar <|> pJTyInt <|> pJTyShort <|> pJTyLong <|> pJTyLongLong <|> pJTySignedUnsigned <|>
             pJTyFloat <|> pJTyDouble <|> pJTyObject

pJExtTy = pJTyAction <|> pJTyBrace

pJTyId = (JTyId <$ pReserved "id") <??> pJTyProtocol
pJTyVoid = JTyVoid <$ pReserved "void"
pJTyBool = JTyBool <$ pReserved "$bool$"
pJTyFloat = JTyFloat <$ pReserved "float"
pJTyDouble = JTyDouble <$ pReserved "double"
pJTyObject = (JTyObject <$> pIdent) <??> pJTyProtocol
pJTyAction = JTyAction <$ pReserved "@action"
pJTyBrace = JTyBrace <$> pPack "{" pJTy "}"

pJTyChar' = JTyChar <$ pReserved "char"
pJTyInt' = JTyInt <$ pReserved "int"
pJTyShort' = JTyShort <$ pReserved "short"
pJTyLong' = JTyLong <$ notFollowedBy (pReserved "long" <* pReserved "long") <* pReserved "long"
pJTyLongLong' = JTyLong <$ pReserved "long" <* pReserved "long"
pTySignUnsign' = pJTyChar' <|> pJTyInt' <|> pJTyShort' <|> pJTyLong' <|> pJTyLongLong'

pJTyChar = flip ($) True <$> pJTyChar'
pJTyInt = flip ($) True <$> pJTyInt'
pJTyShort = flip ($) True <$> pJTyShort'
pJTyLong = flip ($) True <$> pJTyLong'
pJTyLongLong = flip ($) True <$> pJTyLongLong'
pTySignUnsign = pJTyChar <|> pJTyInt <|> pJTyShort <|> pJTyLong <|> pJTyLongLong

pTySimpleSigned = (notFollowedBy pJTySignedUnsigned) *> (JTyInt <$> pJTySigned)
pTySimpleUnsigned = (notFollowedBy pJTySignedUnsigned) *> (JTyInt <$> pJTyUnsigned)

pJTySignedUnsigned = (\s t -> t s) <$> (pJTySigned <|> pJTyUnsigned) <*> pTySignUnsign'
pJTySigned = True <$ pReserved "signed"
pJTyUnsigned = False <$ pReserved "unsigned"

pJTyProtocol = flip JTyProtocol <$ pReserved "<" <*> pIdent <* pReserved ">"

pJTySuffix = pJTyPointer <|> pJTyArray

pJTyPointer = JTyPointer <$ pReserved "*"
pJTyArray = JTyArray <$ pReserved "[" <* pReserved "]"

-- Objective-J: CPString
pEJString :: JsParser Expression
pEJString = EJString <$ pReserved "@" <*> pValToken TkString

-- Objective-J: message
pEJMessage :: JsParser Expression
pEJMessage = notFollowedBy pEArray *> pEJMessage'

pEJMessage' :: JsParser Expression
pEJMessage' = EJMessage <$ pReserved "[" <*> pAssignmentExpression <*>
                ((:) <$> pJArg1 <*> many pJArg) <*> option [] (pComma *> pCommaList pJVarArg) <*
                pReserved "]"

pJArg1 = JArg <$> pStupidIdent <*> pMaybe (pReserved ":" *> pAssignmentExpression)
pJArg = JArg <$> option "" pStupidIdent <* pReserved ":" <*> (Just <$> pAssignmentExpression)
pJVarArg = pAssignmentExpression

pStupidIdent = pIdent <|> pReserved "self" <|> pReserved "new" <|> pReserved "function"

-- Objective-J: import
pJImport :: JsParser SourceElement
pJImport = SEImport <$ pReserved "@import" <*> pValToken TkString

-- Objective-J: implementation
pJImplementation :: JsParser SourceElement
pJImplementation = notFollowedBy (pReserved "@implementation" <* pIdent <* pPack "(" pIdent ")") *>
                     pJImplementation'

pJImplementation' = SEImplementation <$
                     pReserved "@implementation" <*> pIdent <*> pMaybe (pReserved ":" *> pIdent) <*>
                     option [] (pPack "{" (many pJMember)  "}") <*>
                     many pJImplementationElement <*
                     pReserved "@end"

-- Objective-J: category implementation
pJCategory :: JsParser SourceElement
pJCategory = SECategory <$
               pReserved "@implementation" <*> pIdent <*> pPack "(" pIdent ")" <*>
               many pJImplementationElement <*
               pReserved "@end"

-- Objective-J: Member declaration
pJMember :: JsParser JMember
pJMember = JMember <$> pJTy <*> pIdent <*> pMaybe pJAccessors <* pReserved ";"

pJAccessors :: JsParser JAccessors
pJAccessors = (\m -> JAccessors (Map.lookup "property" m)
                                (Map.lookup "getter" m)
                                (Map.lookup "setter" m)
                                (not $ Map.member "readonly" m)
                                (not $ Map.member "writeonly" m)
              )
                <$ pReserved "@accessors" <*> option Map.empty (pPack "(" (pJAccessorArgs) ")")

pJAccessorArgs :: JsParser (Map String Ident)
pJAccessorArgs = Map.fromList <$> pCommaList pJAccessorArg

pJAccessorArg = pJAccessorProp "property" pIdent <|> pJAccessorProp "getter" pSelectorStr <|>
                  pJAccessorProp "setter" pSelectorStr <|> pJAccessorProp' "readonly" <|>
                  pJAccessorProp' "writeonly" <|> pJAccessorProp' "readwrite"


pJAccessorProp n p = (,) <$> pReservedVal TkIdent n <*> option "" (pReserved "=" *> p)
pJAccessorProp' n = flip (,) "" <$> pReservedVal TkIdent n


-- Objective-J: Implementation element
pJImplementationElement :: JsParser JImplementationElement
pJImplementationElement = pJClassMethod <|> pJInstanceMethod

-- Objective-J: Method declaration
pJClassMethod :: JsParser JImplementationElement
pJClassMethod = JClassMethod <$ pReserved "+" <*> pPack "(" pJTy ")" <*>
                  pJMethodParams <*> option False pJVarArgParam <*>
                  pPack "{" pFunctionBody "}"

pJInstanceMethod :: JsParser JImplementationElement
pJInstanceMethod = JInstanceMethod <$ pReserved "-" <*> pPack "(" pJTy ")" <*>
                     pJMethodParams <*> option False pJVarArgParam <*>
                     pPack "{" pFunctionBody "}"

pJMethodParams = (:) <$> pJMethodParam1 <*> many pJMethodParam
pJMethodParam = JMethodParam <$> option "" pStupidIdent <*> (Just <$> pJParam)
pJMethodParam1 = JMethodParam <$> pStupidIdent <*> pMaybe pJParam

pJVarArgParam = True <$ pReserved "," <* pReserved "." <* pReserved "." <* pReserved "."

pJParam = (,) <$ pReserved ":" <*> option (JTyId) (pPack "(" pJTy ")") <*> pIdent

-- Objective-J: selector
pEJSelector :: JsParser Expression
pEJSelector = EJSelector <$ pReserved "@selector" <*>
                pPack "(" pSelector ")"

pSelector :: JsParser [Ident]
pSelector = (:) <$> pIdent <*> pSelectorPart

pSelectorPart = try ((:) <$ pReserved ":" <*> pIdent <*> pSelectorPart) <|>
                ([] <$ pMaybe (pReserved ":"))

pSelectorStr :: JsParser String
pSelectorStr = concat . intersperse ":" <$> pSelector

-------------------------------------------------------------------------------
-- Program parsing
-------------------------------------------------------------------------------

-- FunctionDeclaration (13)
pFunctionDeclaration :: JsParser SourceElement
pFunctionDeclaration = SEFunctionDecl <$ pReserved "function" <*> pIdent <*>
                         pPack "(" (pCommaList pIdent) ")" <*>
                         pPack "{" pFunctionBody "}"

-- Program (14)
pProgram :: JsParser Program
pProgram = Program <$> many pSourceElement

-- SourceElement (14)
pSourceElement :: JsParser SourceElement
pSourceElement = pSEStatement <|> pFunctionDeclaration <|>
                   pJImplementation <|> pJCategory <|> pJImport

pSEStatement = SEStatement <$> pStatement

-------------------------------------------------------------------------------
-- Interface
-------------------------------------------------------------------------------

parser :: JsParser Program
parser = do
           p <- pProgram
           eof
           return p

-- |Produces an abstract syntax tree (AST) given a list of 'Token'
parse :: FilePath               -- ^ The filename the tokens originate from. It
                                --   is solely used for error message purposes
      -> [Token]                -- ^ List of tokens. Usually produced by "Language.JsLib.Scanner"
      -> Either String Program  -- ^ Either an error message or the root element of the AST
parse f tks
  = let result = runParser parser defaultState f tks
    in  case result of
          (Left e) -> Left (show e)
          (Right r) -> Right r
