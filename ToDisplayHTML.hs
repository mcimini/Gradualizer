module ToDisplayHTML where

import Data.List
import Data.String.Utils
import Data.Char
import TypeSystem
import Static
import TypeSystemForCC
import CastInsertion
import GenerateCalculi

sep = "~"
sepNary = "~|~"
castOperator = (Constructor "cast" [Var "E", Var "T1", Var "L", Var "T2"])

toDisplayHTMLRulesIO :: TypeSystem -> IO ()
toDisplayHTMLRulesIO ts = putStrLn (toDisplayHTMLRules ts)

toDisplayHTMLRulesOriginal :: TypeSystem -> String
toDisplayHTMLRulesOriginal (Ts sig rules) = if rules == [] then "" else
	(formatBlockSyntaxOr sig rules "Syntax of the Language") ++ (formatBlockRules typeOf rules' "Type System") ++ (formatBlockRules "step" rules' "Dynamic Semantics") ++ framesReductionIfAny sig rules' where (Ts _ rules') = removePredicateFromTS (Ts sig rules) "value"


framesReductionIfAny sig rules = if (any anyContextInfo sig) then "$$ {" ++ toDisplayHTMLRulesPr (Formula "step" [] [Var "E"] [Var "E'"]) ++ "\\over " ++ toDisplayHTMLRulesPr (Formula "step" [] [Var "f[e]"] [Var "f[e']"]) ++ " } $$" ++ (if doesItAppear "error" (Ts sig rules) then framesError else []) else []

framesError = "$$ {" ++ toDisplayHTMLRulesPr (Formula "step" [] [Var "E"] [Var "err"]) ++ "\\over " ++ toDisplayHTMLRulesPr (Formula "step" [] [Var "f[e]"] [Var "err"]) ++ " } $$"

toDisplayHTMLRules :: TypeSystem -> String
toDisplayHTMLRules (Ts sigAll rulesAll) = (formatBlockSyntaxGr "Syntax of the Gradually Typed Language") ++ (formatBlockRules typeOfGradual rules "Gradual Type System") ++ (displayPatternMatching rules) ++ (formatBlockSyntaxCC sigAll rulesAll "Syntax of the Cast Calculus")  ++ (formatBlockRules typeOfCC rules "Type System of the Cast Calculus") ++ (formatBlockRules typeOfCI rules "Cast Insertion") ++ (formatBlockRules "step" rules "Dynamic Semantics of the Cast Calculus") ++ framesError where (Ts sig rules) = removePredicateFromTS (removePredicateFromTS (Ts sigAll rulesAll) "ground") "value"

formatBlockSyntaxOr :: Signature -> [Rule] -> String -> String
formatBlockSyntaxOr sig rules caption = "<H2> " ++ caption ++ "</H2>$$ \\begin{array}{llcl} " ++ (formatGrammar "Types" "T" "" (map generateCanonical (listOfTypes sig))) ++ (formatGrammar "Expressions" "e" "x" (map generateCanonical (listOfTerms sig))) ++ (formatGrammar "Values" "v" "" (map getTerm (filter (filterRules "value") rules))) ++ (formatGrammar "Errors" "err" "" (map getTerm (filter (filterRules "error") rules))) ++ framesIfAny sig ++ "\\end{array}$$"

framesIfAny sig = if (any anyContextInfo sig) then (formatGrammarFrames "\\Box" (extractContexts sig)) else [] 

-- expression grammar forms used to grab from their typing rule: (map getTerm (filter (filterRules typeOf) rules)))

formatBlockSyntaxGr caption = "<H2> " ++ caption ++ "</H2>$$ \\begin{array}{llcl} " ++  (formatGrammar "Types" "T" "\\ldots" [dyntype]) ++ (formatGrammar "Expressions" "e" "\\ldots \\text{same expressions as the original language}" []) ++ "\\end{array}$$"  -- ("cast", 1, 1)

formatBlockSyntaxCC :: Signature -> [Rule] -> String -> String
formatBlockSyntaxCC sig rules caption = "<H2> " ++ caption ++ "</H2>$$ \\begin{array}{llcl} " ++ (formatGrammar "Types" "T" "\\ldots" [dyntype]) ++ (formatGrammar "Expressions" "e" "\\ldots" [castOperator]) ++ (formatGrammar "Ground Types" "G" "\\ldots" (map getTerm (filter (filterRules "ground") rules))) ++ (formatGrammar "Values" "v" "\\ldots" (map getTerm (filter (filterRules "value") rules))) ++ (formatGrammar "Errors" "err" "\\ldots" (map getTerm (filter (filterRules "error") rules))) ++ castFrame sig ++ "\\end{array}$$"

castFrame sig = if (any anyContextInfo (deleteDeclByName sig "cast")) then "\\text{ Frame }  & f & ::= & \\ldots \\mid " ++ (toDisplayHTMLRulesTerm (Constructor "cast" [Var "\\Box", Var "T1", Var "L", Var "T2"] ) ) else []
-- ++ (formatGrammarFrames "\\ldots" []) ++ "\\end{array}$$"


formatGrammar :: String -> String -> String -> [Term] -> String
formatGrammar category nominal first terms = if first == "" && terms == [] then "" else let textwrap = \s -> "\\text{" ++ s ++ "}" in (textwrap category) ++ " & " ++ (nominal) ++ " & ::= & " ++ (intercalate "\\mid" (filter (not . null) ([first] ++ (map toDisplayHTMLRulesTerm terms)))) ++ " \\\\ "

formatGrammarFrames :: String -> [(String, (Int, [Int]), Int)] -> String
formatGrammarFrames first pairs = if first == "" && pairs == [] then "" else let textwrap = \s -> "\\text{" ++ s ++ "}" in (textwrap "Frames") ++ " & " ++ "f" ++ " & ::= & " ++ (intercalate "\\mid" (filter (not . null) ([first] ++ (map toDisplayHTMLRulesTerm (map toContextTerm pairs))))) ++ " \\\\ " where toContextTerm = \pair -> case pair of (c, (n,values) , max) -> (Constructor c (map (\i -> if n == i then Var "\\Box" else if elem i values then Var ("V" ++ show i) else Var ("E" ++ show i)) [1.. max])) 

formatBlockRules pred rules caption = "<H2> " ++ caption ++ "</H2>" ++ firstrule ++ (unlines (map toDisplayHTMLRulesR (filter (filterRules pred) rules))) where firstrule = case lookup pred axioms of { Just something -> something ; Nothing -> "" }

displayPatternMatching rules = (unlines (map toDisplayHTMLRulesR (filter (filterRules "match") rules)))

toDisplayHTMLRulesR :: Rule -> String 
toDisplayHTMLRulesR (Rule premises conclusion) = 
			"$$ {" ++ (intercalate "\\qquad " (map toDisplayHTMLRulesPr premises)) ++ "\\over " ++ (toDisplayHTMLRulesPr conclusion) ++ " } $$"
			 
toDisplayHTMLRulesPr :: Premise -> String
toDisplayHTMLRulesPr (Formula pred info interms outterms) = case (lookup pred symbols) of {Nothing -> displayOrdinaryPred pred info interms outterms ; Just handler -> handler info interms outterms }											
toDisplayHTMLRulesPr (Hypothetical premise1 premise2) = case premise1 of (Formula pred info interms outterms) -> "x:" ++ (toDisplayHTMLRulesTerm (last outterms)) ++ ", " ++ (toDisplayHTMLRulesPr premise2) 
toDisplayHTMLRulesPr (Negated premise) = "\\lnot (" ++ (toDisplayHTMLRulesPr premise) ++ ")"

toDisplayHTMLRulesTerm :: Term -> String
toDisplayHTMLRulesTerm (Var varterm) = suitable_transformation varterm
toDisplayHTMLRulesTerm (Constructor c terms) = case c of
   "dyn" -> "\\star"
   "cast" -> "(" ++ (toDisplayHTMLRulesTerm (terms !! 0)) ++ ":" ++ (toDisplayHTMLRulesTerm (terms !! 1)) ++ "\\Rightarrow^{" ++ (toDisplayHTMLRulesTerm (terms !! 2)) ++ "} " ++ (toDisplayHTMLRulesTerm (terms !! 3)) ++ ")"
   "entry" -> (toDisplayHTMLRulesTerm (terms !! 2)) ++ "[" ++ (toDisplayHTMLRulesTerm (terms !! 0)) ++ "\\mapsto "++ (toDisplayHTMLRulesTerm (terms !! 1)) ++ "]" 
   otherwise -> if terms == [] then "\\mathrm{" ++ c ++ "}" else "(\\mathrm{" ++ c ++ "}\\," ++ (intercalate sep (map toDisplayHTMLRulesTerm terms)) ++ ")"
toDisplayHTMLRulesTerm (Application term1 term2) = toDisplayHTMLRulesTerm term1 ++ substitution where substitution = if term2 == (Var "x") then "" else "[" ++ toDisplayHTMLRulesTerm term2 ++ "/x]"
toDisplayHTMLRulesTerm (Bound x) = x
toDisplayHTMLRulesTerm (Lambda boundvar term) = toDisplayHTMLRulesTerm term 
{-
For true lambda-prolog syntax over HOAS: 
toDisplayHTMLRulesTerm (Application term1 term2) = toDisplayHTMLRulesTerm term1 -- "(" ++ toDisplayHTMLRulesTerm term1 ++ sep ++ toDisplayHTMLRulesTerm term2 ++ ")"
toDisplayHTMLRulesTerm (Bound x) = x
toDisplayHTMLRulesTerm (Lambda boundvar term) = "(\\lambda " ++ boundvar ++ ".\\, " ++ toDisplayHTMLRulesTerm term ++ ")"
-}

displayOrdinaryPred pred info interms outterms = ("\\mathrm{" ++ (pred ++ (firstCharUP (concat info))) ++ "}") ++ "\\," ++ (intercalate sep (map toDisplayHTMLRulesTerm interms)) ++ sep ++ (intercalate sep (map toDisplayHTMLRulesTerm outterms))
displayOrdinaryTypeOf pred info interms outterms = "\\Gamma " ++ pred ++ "\\," ++ (intercalate sep (map toDisplayHTMLRulesTerm interms)) ++ ":" ++ (intercalate sep (map toDisplayHTMLRulesTerm outterms))
displayInfixIO pred info interms outterms = (intercalate sepNary (map toDisplayHTMLRulesTerm interms)) ++ pred ++ (intercalate sepNary (map toDisplayHTMLRulesTerm outterms))
displayBinaryInfixII pred info interms outterms = (toDisplayHTMLRulesTerm (head interms)) ++ pred ++ (toDisplayHTMLRulesTerm (interms !! 1))

symbols :: [(String, ([String] -> [Term] -> [Term] -> String))]
symbols = 
     [(typeOf, displayOrdinaryTypeOf "\\vdash "),
     (typeOfGradual, displayOrdinaryTypeOf "\\vdash_{Gr} "),
     (typeOfCC, displayOrdinaryTypeOf "\\vdash_{CC} "),
     (typeOfCI, \info -> \interms -> \outterms -> "\\Gamma \\vdash_{CI} " ++ (intercalate sep (map toDisplayHTMLRulesTerm interms)) ++ "\\hookrightarrow " ++ (toDisplayHTMLRulesTerm (outterms !! 0)) ++ ":" ++ (toDisplayHTMLRulesTerm (outterms !! 1))),
     ("step", displayInfixIO "\\leadsto "),
     ("step'", displayInfixIO "\\leadsto_{c} "),
     ("flow", displayBinaryInfixII "\\sim "),
     ("=", displayBinaryInfixII " = "),
     ("match", \info -> \interms -> \outterms -> (toDisplayHTMLRulesTerm (head interms)) ++ "\\mathrel{\\triangleright} " ++ (toDisplayHTMLRulesTerm (Constructor (concat info) outterms))),
     ("consistency", displayBinaryInfixII "\\approx "),
     ("subtype", displayBinaryInfixII "<:"),
     ("subtypeG", displayBinaryInfixII "\\lesssim "),
     ("subtypeCI", \info -> \interms -> \outterms -> (toDisplayHTMLRulesTerm (outterms !! 0)) ++ "\\vdash " ++ (displayBinaryInfixII "\\lesssim " info interms outterms)),
     ("fresh", displayBinaryInfixII "\\textrm{ fresh in }"),
     ("lookup", \info -> \interms -> \outterms -> (toDisplayHTMLRulesTerm (interms !! 1)) ++ "( " ++ (toDisplayHTMLRulesTerm (interms !! 0)) ++ ") = " ++ (toDisplayHTMLRulesTerm (outterms !! 0)))
     ]

{-
predicate ++ "\\," ++ (intercalate sep (map toDisplayHTMLRulesTerm interms)) ++ sep ++ (intercalate sep (map toDisplayHTMLRulesTerm outterms)) where predicate = symbols :: [(String, String)]
symbols = 
     [(typeOf, "\\vdash"),
     (typeOfGradual, "\\vdash_{Gr}"),
     (typeOfCC, "\\vdash_{CC}"),
     (typeOfCI, "\\vdash_{CI}")]
-}

getTerm :: Rule -> Term
getTerm (Rule premises conclusion) = getFirstInputOfPremise conclusion
--getTerm rule = error (show rule)

getType :: Rule -> Term
getType (Rule premises conclusion) = getFirstOutputOfPremise conclusion

generateCanonical :: SignatureEntry -> Term
generateCanonical (Decl c typ contraInfo contexts entries) = (Constructor c (map symbolByEntry entries)) where symbolByEntry = \entry -> case entry of {Simple "typ" -> Var ("T") ; Simple "term" -> Var ("E") ; Abs something "typ" -> Var ("T") ; Abs something "term" -> Var ("E") ; otherwise -> Var "X"}

suitable_transformation :: String -> String
suitable_transformation variable = let chop = (last variable) in let butLast = (take ((length variable) -1) variable) in if [chop] == "'" then (suitable_transformation butLast) ++ "'" else if isDigit chop then (suitable_transformation butLast) ++ "_" ++ [chop] else if startswith "V" variable || startswith "E" variable || startswith "L" variable then (map toLower variable) else variable 

--let numero = (length variable) -1 in if isDigit (last variable) then (take numero normalized) ++ "_" ++ [(last variable)] else normalized where normalized = if startswith "V" variable || startswith "E" variable then (map toLower variable) else variable

axioms :: [(String, String)]
axioms = [
      (typeOf, "$$ { x:T, \\Gamma \\vdash x:T } $$"),
      (typeOfGradual, "$$ { x:T, \\Gamma \\vdash_{Gr} x:T } $$"),
      (typeOfCC, "$$ { x:T, \\Gamma \\vdash_{CC} x:T } $$"),
      (typeOfCI, "$$ { x:T, \\Gamma \\vdash_{CI} x \\hookrightarrow x :T } $$")
      ]