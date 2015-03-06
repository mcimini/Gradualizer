module FromLambdaProlog (parseLP) where

import TypeSystem
import InputTypes
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token


-- Lexer

{-
'pi x\\ typeOf'   { TokenHyp1 }
id   { TokenId $$ }
var   { TokenVar $$ }
'=> typeOf'  { TokenHyp2 }
'('   { TokenLParen }
')'   { TokenRParen }
','   { TokenComma }
'.'   { TokenDot }
':-'  { TokenTurnStyle }
'x'  { TokenBound }
-}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedNames = ["pi x\\ typeOf","=> typeOf",":-",".","(",")","x"],
          Token.commentLine = "#" }

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser String
identifier = Token.identifier lexer

conId :: Parser String
conId = do { c <- lower
           ; cs <- many alphaNum
           ; return $ (c:cs) }

-- Parser
{-
variable :: Parser Term
variable = Var `fmap` identifier
-}

tsParse :: Parser TypeSystem
tsParse = do 
	rules <- many ruleGram
	return (Ts [] rules)

ruleP :: Parser Rule
ruleP = do
	pred <- Token.lexeme lexer conId
	tterm <- term
	ttype <- term	
  	reserved ":-"
	prems <- Token.commaSep1 lexer premise
	reserved "."
	return (Rule prems tterm ttype)

fact :: Parser Rule
fact = do
	pred <- Token.lexeme lexer conId
	tterm <- term
	ttype <- term	
  	reserved "."
	return (Rule [] tterm ttype)

ruleGram :: Parser Rule
ruleGram = try fact <|> ruleP 


formula :: Parser Premise
formula = do
	pred <- Token.lexeme lexer conId
  	tterm <- term
  	ttype <- term
  	return (Formula pred [] [tterm] [ttype])

hypothetical :: Parser Premise
hypothetical = do
	reserved "(pi x\\ (typeOf x"
	ttypeIn <- term
	reserved "=> typeOf"
	tterm <- term
	ttype <- term
	reserved ")"
	reserved ")"
	return (Hypothetical "x" (Var "x") ttypeIn tterm ttype)

premise :: Parser Premise
premise = formula <|> hypothetical <|> parens formula

constructor :: Parser Term
constructor = do { 	c <- Token.lexeme lexer conId
	; terms <- many term
	; if (hasInputTypes c) then return (Constructor c [(head terms)] (tail terms)) else return (Constructor c [] terms) }

application :: Parser Term
application = do {
{- 	reserved "(" ; -}
	tterm1 <- Token.lexeme lexer varId
	; tterm2 <- boundVar
{- 	; reserved ")" -}
	; return (Application (Var tterm1) tterm2)}

boundVar :: Parser Term
boundVar = do { reserved "x" 
			; return (Var "x")}

variable :: Parser Term
variable = do 
	name <- Token.lexeme lexer varId
	return (Var name)

varId :: Parser String
varId = do { c <- upper
           ; cs <- many alphaNum
           ; return (c:cs) }

{-
withParens :: Parser Term
withParens = do
	reserved "("
	tterm <- term
	reserved ")"
	return tterm
-}
	
term :: Parser Term
term = try application <|> constructor <|> variable <|> boundVar <|> parens term
{- parens term <?> "atom" -}

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseLP :: String -> TypeSystem
parseLP t = 
  case parse (allOf tsParse) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast
