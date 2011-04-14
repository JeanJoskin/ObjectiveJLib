{
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

module Language.JsLib.Scanner.Scanner (scan) where

import Text.ParserCombinators.Parsec.Pos
import Language.JsLib.Scanner.Tokens
}
-------------------------------------------------------------------------------
-- Character groups
-------------------------------------------------------------------------------
-- TODO: support Unicode

$letter = [A-Za-z]
$combiningMark = []
$digit = [0-9]
$connectorPunctuation = [_]
$lineTerminator = [\n\r]
$hexDigit = [0-9a-fA-F]

-------------------------------------------------------------------------------
-- Macro's
-------------------------------------------------------------------------------

@unicodeEscapeSequence = u $hexDigit{4}

-- Identifier
@identifierStart = $letter | [\$\_] | \\ @unicodeEscapeSequence
@identifierPart = @identifierStart | $combiningMark | $digit | $connectorPunctuation
@identifier = @identifierStart @identifierPart+ | @identifierPart+

-- Keywords
@keyword = 
    break|do|instanceof|typeof|case|else|new|var|catch|finally|return|void|
    continue|for|switch|while|debugger|function|this|with|default|if|throw|
    delete|in|try

@objectiveJKeyword = "@implementation" | "@end" | "@import" | "@accessors" |
                       "id" | "@"

-- Punctuators
@punctuator =
    "[" | "]" | "(" | ")" | "{" | "}" |
    ">=" | "<" | ">" | "," | ";" | "." |
    "!==" | "===" | "!=" | "==" | "<=" |
    "--" | "++" | "%" | "*" | "-" | "+" |
    "^" | "|" | "&" | ">>>" | "<<" | ">>" |
    ":" | "?" | "||" | "&&" | "~" | "!" |
    ">>=" | "%=" | "*=" | "-=" | "+=" | "=" |
    "^=" | "|=" | "&="  | ">>>=" | "<<="

@divPunctuator =
    "/" | "/="

-- Comment
@multiLineComment = \/\* ([^\*] | \*+[^\/\*] | \*\n | \n)* \*+\/
@singleLineComment = \/\/ ~$lineTerminator*
@comment = @singleLineComment | @multiLineComment

-- String
@stringEscape  = [^0-9XxUu] | @unicodeEscapeSequence | [xX] $hexDigit{2}
@stringDoubleQ  =  [^\"\\] | \\ @stringEscape
@stringSingleQ  =  [^\'\\] | \\ @stringEscape

-- Numbers
@decimalLiteral = [0-9]+ (\.[0-9]+)? ([eE] [\+\-]? [0-9]+)? |
                  \.[0-9]+ ([eE] [\+\-]? [0-9]+)?

@hexLiteral = 0[xX] $hexDigit+

-- Regular expressions
$regExpNT = .
@regExpEscape = \\ $regExpNT
@regExpClass = \[ ($regExpNT # [\]\\] | @regExpEscape)+ \]
@regExpChar = $regExpNT # [\\\/\[] | @regExpEscape | @regExpClass
@regExpFirstChar = $regExpNT # [\*\\\/\[] | @regExpEscape | @regExpClass

@regExpLiteral = \/ @regExpFirstChar @regExpChar* \/ @identifierPart*

-- Import path
@importPath =  \" [^\"]* \" | \< [^\>]* \>

-------------------------------------------------------------------------------
-- Token definitions
-------------------------------------------------------------------------------

tokens :-
  <0,sdiv> \" @stringDoubleQ* \"             { ValToken TkString }
  <0,sdiv> \' @stringSingleQ* \'             { ValToken TkString }
  <0,sdiv> @keyword | @objectiveJKeyword     { Reserved }
  <0,sdiv> @punctuator                       { Reserved }
  <sdiv>   @divPunctuator                    { Reserved }
  <simp>   @importPath                       { ValToken TkString }
  <0,sdiv> @comment                          ;
  <0,sdiv> true | false                      { Reserved }
  <0,sdiv> null                              { Reserved }
  <0,sdiv> @decimalLiteral                   { ValToken TkNumeric }
  <0,sdiv> @hexLiteral                       { ValToken TkNumeric }
  <0,sdiv> @identifier                       { ValToken TkIdent }
  <0>      @regExpLiteral                    { ValToken TkRegExp }
  <0,sdiv,simp> $white+                      ;

{
type AlexInput = (SourcePos, String)
type StateStack = [Int]

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_, [])   = Nothing
alexGetChar (p, c:cs) = Just (c, (updatePosChar p c, cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: should not be used."

sdivTriggers = [ "]", ")", "++", "--" ]

-- |Updates the scanner state. The state is changed to 0 when we can expect
-- |a regExp and switched to sdiv when we expect a division operator to handle
-- |ambiguities in the syntax (e.g. is /b/c a regExp or a division
-- |expression?).
updateState :: Token  -- ^ The current token
            -> Int    -- ^ The current state
            -> Int    -- ^ The new state
updateState (ValToken ty _ _) s = case ty of
                                    TkComment -> s
                                    _         -> sdiv
updateState (Reserved r _) s | elem r sdivTriggers = sdiv
                             | r == "@import"      = simp
                             | otherwise           = 0

-- |Chops a String into Tokens ignoring whitespace
scan :: FilePath   -- ^ The filename the string originates from. It is solely
                   --   used for error message purposes
     -> String     -- ^ The String to be processed
     -> [Token]    -- ^ A list of tokens
scan f s = scan' (initialPos f,s) 0

scan' :: AlexInput -> Int -> [Token]
scan' i@(pos,str) s =
  case (alexScan i s) of
    (AlexEOF)                 -> []
    (AlexError _)            -> let (Just (c, i')) = alexGetChar i
                                 in  errToken [c] pos : scan' i' s
    (AlexSkip i' len)         -> scan' i' s
    (AlexToken i' len action) -> let token = action (take len str) pos
                                 in  token : scan' i' (updateState token s)
}
