module FromLambdaProlog (parseLP) where

import Prelude
import Data.String.Utils
import TypeSystem
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

data LogicalEntry = ConstrL String
                  | DeconstrL LogicalLine
                  | DerivedL 
                  | DerivedDecL LogicalLine
               deriving (Show, Eq, Ord)

type ModeTable = [(String, Int)]

contraTag = "% operatorInfo"
modeTag = "% mode"
contextTag = "% context"
inputKeyword = "inp"


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
          Token.reservedNames = ["pi x\\ typeOf","=> typeOf",":-",".","(",")","x","type","->", contraTag, modeTag, modeTag, inputKeyword, "[", "]"]}

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

number :: Parser Int
number = do { cs <- many1 digit
            ; return $ read cs }
numberBracket :: Parser Int
numberBracket = do { cs <- many1 digit
			  ; _ <- char '['
            ; return $ read cs }

-- Parser
{-
variable :: Parser Term
variable = Var `fmap` identifier
-}

-- (map (adjustByContra contraEntries) sig) is the adjusted sig.
tsParse :: Parser TypeSystem 
tsParse = do { sig <- many signature
             ; contraEntries <- many contravarTags
             ; contexts <- many contextTags
             ; modes <- many modeTags
	     ; rules <- many (ruleGram modes)
             ; let newsig = (adjustSignature contraEntries contexts sig)
	     ; return $ (Ts newsig (rules ++ (completeRules (Ts newsig rules) contexts))) }
        
contravarTags :: Parser (String, LogicalEntry)
contravarTags = do {
        ; reserved contraTag
        ; c <- Token.lexeme lexer conId
        ; reserved ":-"
        ; info <- Token.commaSep1 lexer (many alphaNum)
		; reserved "."
        ; (case (info !! 0) of
          "constructor" -> return (c, (ConstrL (info !! 1)))
          "deconstructor" -> if (length info) == 4 then return (c, (DeconstrL ((info !! 1),(read (info !! 2)::Int, 0)))) else return (c, (DeconstrL ((info !! 1),(read (info !! 2)::Int, read (info !! 4)::Int))))
          "derived" -> if (length info) == 1 then return (c, (DerivedL)) else return (c, (DerivedDecL ((info !! 1),(read (info !! 2)::Int, read (info !! 4)::Int))))) }


-- assumption: all the inputs are used at the beginning. 
modeTags :: Parser (String, Int)
modeTags = do {
        ; reserved modeTag
        ; c <- Token.lexeme lexer conId
        ; modes <- sepBy conId (do {Token.whiteSpace lexer; string "->"; Token.whiteSpace lexer})
  		; reserved "."
    	; let numInputs = length (filter (== inputKeyword) modes)
        ; return (c, numInputs::Int) }

contextTags :: Parser (String, [(Int, [Int])])
contextTags = do {
        ; reserved contextTag
        ; c <- Token.lexeme lexer conId
        ; positions <- Token.commaSep1 lexer ctx_position
  		; reserved "."
        ; return (c, positions) }

ctx_position :: Parser (Int, [Int])
ctx_position = do {
        ; pos <- numberBracket -- Token.lexeme lexer number
--        ; reserved "["
        ; values <- Token.commaSep lexer number
		; reserved "]"
		; return (pos, values) }


ruleP :: ModeTable -> Parser Rule
ruleP modes = do
        frm <- formula modes
        reserved ":-"
	prems <- Token.commaSep1 lexer (premise modes)
	reserved "."
        return (Rule prems frm) 

fact :: ModeTable -> Parser Rule
fact modes = do
	frm <- formula modes
  	reserved "."
	return (Rule [] frm)

ruleGram :: ModeTable -> Parser Rule
ruleGram modes = try (fact modes) <|> (ruleP modes)

formula :: ModeTable -> Parser Premise
formula modes = do
	pred <- Token.lexeme lexer conId
	tterms <- many term 
        case lookup pred modes of {Nothing -> return (Formula pred [] tterms []) ; Just numero -> return (Formula pred [] (take numero tterms) (drop numero tterms))}

hypothetical :: Parser Premise
hypothetical = do
	reserved "(pi x\\ (typeOf x"
	ttypeIn <- term 
	reserved "=> typeOf"
	tterm <- term 
	ttype <- term 
	reserved ")"
	reserved ")"
	return (Hypothetical (Formula typeOf [] [(Bound "x")] [ttypeIn]) (Formula typeOf [] [tterm] [ttype]))

premise :: ModeTable -> Parser Premise
premise modes = formula modes <|> hypothetical <|> parens (formula modes)

constructor :: Parser Term
constructor = do { 	c <- Token.lexeme lexer conId
	; terms <- many term 
	--	; if (isTypeConstructor pkg c) then return (Constructor c (take (contravariantArguments pkg c) terms) (drop (contravariantArguments pkg c) terms)) else return (Constructor c (take (userTypeArguments pkg c) terms) (drop (userTypeArguments pkg c) terms)) }
	; return (Constructor c terms) }

application :: Parser Term
application = do {
{- 	reserved "(" ; -}
	tterm1 <- Token.lexeme lexer varId
	; tterm2 <- try boundVar <|> variable
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
	
term ::  Parser Term
term = try (parens application) <|> constructor <|> variable <|> boundVar <|> parens term
{- parens term <?> "atom" -}

allOf :: Parser a -> Parser a
allOf p = do
  (Token.whiteSpace lexer)
  r <- p
  eof
  return r

parseLP :: [String] -> TypeSystem
parseLP t = let stuffToParse = unlines (filter (not . parseGarbage) t) in 
  case parse (allOf tsParse) "stdin" stuffToParse of
    Left err -> error (show err)
    Right ast -> ast

parseGarbage :: String -> Bool
parseGarbage line = startswith "sig" line || startswith "kind" line || startswith "module" line || startswith "\n" line || (startswith "%" line && (not ((startswith contraTag line || (startswith modeTag line) || (startswith contextTag line)))))

signature :: Parser SignatureEntry
signature = do
        reserved "type"
        c <- Token.lexeme lexer conId
        entries <- sepBy typeEntry (do {Token.whiteSpace lexer; string "->"; Token.whiteSpace lexer})
  	reserved "."
        return (Decl c (toStringTE (last entries)) Nothing Nothing (init entries))

typeEntry :: Parser TypeEntry
typeEntry = try simple <|> parens hoas 

simple :: Parser TypeEntry
simple = do
        item <- Token.lexeme lexer conId
        return (Simple item)

hoas :: Parser TypeEntry
hoas = do
        items <- sepBy (Token.lexeme lexer conId) (do {Token.whiteSpace lexer; string "->"; Token.whiteSpace lexer})
        if (length items) == 2 then return (Abs (items !! 0) (items !! 1)) else error "Parsing Error: At the moment, The use of HOAS is restriced to only abstractions of the form (type1 -> type2) (i.e. only one abstracted argument) "

adjustSignature :: [(String, LogicalEntry)] -> [(String, [(Int, [Int])])] -> Signature -> Signature
adjustSignature contraEntries contexts sig = adjustByContext contexts (adjustByContra contraEntries sig)

adjustByContra :: [(String, LogicalEntry)] -> Signature -> Signature
adjustByContra [] sig = sig
adjustByContra (entry:rest) sig = case entry of (c, info) -> case (searchDeclByName sig c) of { Nothing -> error ("ERROR: adjustByContra" ++ (show sig) ++ c) ; Just (Decl c1 typ contraInfo contexts entries) -> (Decl c1 typ (addInfo contraInfo info) contexts entries):(adjustByContra rest (deleteDeclByName sig c)) }
               where
               addInfo = \contra -> \info -> case contra of {Nothing -> Just (newInfo info) ; Just (Deconstructor info2) -> Just (Deconstructor ((extract info):info2)) }
               newInfo = \info -> case info of { DeconstrL info2 -> Deconstructor [info2] ; ConstrL c -> Constr c ; DerivedDecL info2 -> DerivedDec [info2] ; DerivedL -> Derived }
               extract = \info -> case info of DeconstrL info2 -> info2

adjustByContext :: [(String, [(Int, [Int])])] -> Signature -> Signature
adjustByContext [] sig = sig
adjustByContext (entry:rest) sig = case entry of (c, positions) -> case (searchDeclByName sig c) of { Nothing -> error ("ERROR: adjustByContext" ++ (show sig) ++ c) ; Just (Decl c1 typ contraInfo contextInfo entries) -> (Decl c1 typ contraInfo (Just positions) entries):(adjustByContext rest (deleteDeclByName sig c)) }


completeRules :: TypeSystem -> [(String, [(Int, [Int])])] -> [Rule]
completeRules ts contexts = []

{-
completeRules :: TypeSystem -> [(String, [Int])] -> [Rule]
completeRules ts contexts = case ts of (Ts sig rules) -> let subtype = if any (doesItAppear_rule "subtype") rules then (subtypeRules sig) else [] in subtype:(concat (map (contextualRules sig) contexts))

contextualRules :: Signature -> (String, [Int]) -> [Rule]
contextualRules sig pair = case pair of (c, positions) -> case (searchDeclByName sig c) of { Nothing -> error ("ERROR: adjustByContext" ++ (show sig) ++ c) ; Just (Decl c1 typ contraInfo contextInfo entries) -> map singleRule positions where singleRule = \n 
-}