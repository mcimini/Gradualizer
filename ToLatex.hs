module ToLatex where

import Data.List
import Data.String.Utils
import Data.Char
import TypeSystem
import Static
import TypeSystemForCC
import CastInsertion
import GenerateCalculi

vspace = "\\\\[2ex]"
sep = "~"
castOperator = (Constructor "cast" [Var "E", Var "T1", Var "L", Var "T2"])

toLatexALL :: String -> TypeSystem -> TypeSystem -> String 
toLatexALL title tsOriginal ts = makeSection title ++ toLatexOriginal title tsOriginal ++ toLatexGradual title tsOriginal ts  ++ toLatexCastCalculus title (extend tsOriginal ts) ++ "\n\n"

toLatexOriginal :: String -> TypeSystem -> String
toLatexOriginal title (Ts sigOr rulesOr) = makeSubSection title ++ (formatBlockSyntax sigOr rulesOr) ++ (formatBlockRules typeOf rules "Type System" "$\\Gamma \\vdash \\,e:T$") ++ (formatBlockRules "step" rules "Dynamic Semantics" "$e \\leadsto e$") where (Ts sig rules) = removePredicateFromTS (removePredicateFromTS (Ts sigOr rulesOr) "ground") "value"

toLatexGradual :: String -> TypeSystem -> TypeSystem -> String
toLatexGradual title (Ts sigOr rulesOr) (Ts sigGr rulesGr) = makeSubSection (gradualTitle title) ++ (formatBlockSyntax (sigOr ++ sig_dyntype) rulesOr) ++ (formatBlockRules typeOfGradual rulesGr "Gradual Type System" "$\\Gamma \\typeOfG \\,e:T$") ++ (formatBlockRules "match" rulesGr "Pattern-matching" "$T\\mathrel{\\triangleright}T$") 

toLatexCastCalculus :: String -> TypeSystem -> String
toLatexCastCalculus title (Ts sigAll rulesAll) = makeSubSection (castTitle title) ++ (formatBlockSyntax sigAll rulesAll )  ++ (formatBlockRules typeOfCC rulesAll "Type System of the Cast Calculus" "$\\Gamma \\typeOfCC \\,e:T$") ++ (formatBlockRules typeOfCI rulesAll "Cast Insertion" "$\\Gamma \\typeOfCI e\\hookrightarrow e:T$") ++ (formatBlockRules "step" rules "Dynamic Semantics of the Cast Calculus" "$e \\leadsto e$") where (Ts sig rules) = removePredicateFromTS (removePredicateFromTS (Ts sigAll rulesAll) "ground") "value"

formatBlockSyntax :: Signature -> [Rule] -> String
formatBlockSyntax sig rules = "$$ \\begin{array}{llcl}\n " ++ (formatGrammar "Types" "T" "" (map generateCanonical (listOfTypes sig))) ++ (formatGrammar "Expressions" "e" "x" (map generateCanonical (listOfTerms sig))) ++ (formatGrammar "Ground Types" "G" "" (map getTerm (filter (filterRules "ground") rules))) ++ (formatGrammar "Values" "v" "" (map getTerm (filter (filterRules "value") rules))) ++ (formatGrammar "Errors" "err" "" (map getTerm (filter (filterRules "error") rules))) ++ (formatGrammarFrames "\\Box" (extractContexts sig)) ++ "\\end{array}$$"

-- expression grammar forms used to grab from their typing rule: (map getTerm (filter (filterRules typeOf) rules)))
formatBlockSyntaxGr :: Signature -> [Rule] -> String
formatBlockSyntaxGr sig rules = "$$ \\begin{array}{llcl} " ++  (formatGrammar "Types" "T" "" ((map generateCanonical (listOfTypes sig)) ++ [dyntype])) ++ (formatGrammar "Expressions" "e" "x" (map generateCanonical (listOfTerms sig))) ++ (formatGrammar "Values" "v" "" (map getTerm (filter (filterRules "value") rules))) ++ (formatGrammar "Errors" "err" "" (map getTerm (filter (filterRules "error") rules))) ++ "\\end{array}$$"

formatBlockSyntaxCC :: Signature -> [Rule] -> String
formatBlockSyntaxCC sig rules = "$$ \\begin{array}{llcl} " ++ (formatGrammar "Types" "T" "" ((map generateCanonical (listOfTypes sig)) ++ [dyntype])) ++ (formatGrammar "Expressions" "e" "x" ((map generateCanonical (listOfTerms sig)) ++ [castOperator])) ++ (formatGrammar "Ground Types" "G" "" (map getTerm (filter (filterRules "ground") rules))) ++ (formatGrammar "Values" "v" "" (map getTerm (filter (filterRules "value") rules))) ++ (formatGrammar "Errors" "err" "" (map getTerm (filter (filterRules "error") rules))) ++ "\\end{array}$$"


formatGrammar :: String -> String -> String -> [Term] -> String
formatGrammar category nominal first terms = if terms == [] then "" else let textwrap = \s -> "\\text{" ++ s ++ "}" in (textwrap category) ++ " & " ++ (nominal) ++ " & ::= & " ++ (intercalate "\\mid" (filter (not . null) ([first] ++ (map toLatexRulesTerm terms)))) ++ " \\\\ \n"

formatGrammarFrames :: String -> [(String, (Int, [Int]), Int)] -> String
formatGrammarFrames first pairs = if first == "" && pairs == [] then "" else let textwrap = \s -> "\\text{" ++ s ++ "}" in (textwrap "Frames") ++ " & " ++ "f" ++ " & ::= & " ++ (intercalate "\\mid" (filter (not . null) ([first] ++ (map toLatexRulesTerm (map toContextTerm pairs))))) ++ " \\\\ " where toContextTerm = \pair -> case pair of (c, (n,values) , max) -> (Constructor c (map (\i -> if elem i values then Var ("E" ++ show i) else if i < n then Var ("V" ++ show i) else Var "\\Box") [1.. max])) 

formatBlockRules pred rules caption math = caption ++ " \\hfill \\fbox{" ++ math ++  "} \\\\[-3ex]\n\\begin{gather*}\n" ++ firstrule ++ (intercalate vspace (map toLatexRulesR (filter (filterRules pred) rules))) ++ "\\end{gather*}\n" where firstrule = case lookup pred axioms of { Just something -> something ++ vspace ; Nothing -> "" }

displayPatternMatching rules = (unlines (map toLatexRulesR (filter (filterRules "match") rules)))

toLatexRulesR :: Rule -> String 
toLatexRulesR (Rule premises conclusion) = if premises == [] then " {" ++ (toLatexRulesPr conclusion)  ++ " } "  else 
			" {" ++ (intercalate "\\qquad " (map toLatexRulesPr premises)) ++ "\\over " ++ (toLatexRulesPr conclusion) ++ " } "  
			 
toLatexRulesPr :: Premise -> String
toLatexRulesPr (Formula pred info interms outterms) = case (lookup pred symbols) of {Nothing -> displayOrdinaryPred pred info interms outterms ; Just handler -> handler info interms outterms }											
toLatexRulesPr (Hypothetical premise1 premise2) = case premise1 of (Formula pred info interms outterms) -> "x:" ++ (toLatexRulesTerm (last outterms)) ++ ", " ++ (toLatexRulesPr premise2) 
toLatexRulesPr (Negated premise) = "\\lnot (" ++ (toLatexRulesPr premise) ++ ")"

toLatexRulesTerm :: Term -> String
toLatexRulesTerm (Var varterm) = suitable_transformation varterm
toLatexRulesTerm (Constructor c terms) = case c of
   "dyn" -> "\\star"
   "cast" -> "(" ++ (toLatexRulesTerm (terms !! 0)) ++ ":" ++ (toLatexRulesTerm (terms !! 1)) ++ "\\Rightarrow^{" ++ (toLatexRulesTerm (terms !! 2)) ++ "} " ++ (toLatexRulesTerm (terms !! 3)) ++ ")"
   otherwise -> if terms == [] then "\\mathrm{" ++ c ++ "}" else "(\\mathrm{" ++ c ++ "}\\," ++ (intercalate sep (map toLatexRulesTerm terms)) ++ ")"
toLatexRulesTerm (Application term1 term2) = toLatexRulesTerm term1 ++ substitution where substitution = if term2 == (Var "x") then "" else "[" ++ toLatexRulesTerm term2 ++ "/x]"
toLatexRulesTerm (Bound x) = x
toLatexRulesTerm (Lambda boundvar term) = toLatexRulesTerm term 
{-
For true lambda-prolog syntax over HOAS: 
toLatexRulesTerm (Application term1 term2) = toLatexRulesTerm term1 -- "(" ++ toLatexRulesTerm term1 ++ sep ++ toLatexRulesTerm term2 ++ ")"
toLatexRulesTerm (Bound x) = x
toLatexRulesTerm (Lambda boundvar term) = "(\\lambda " ++ boundvar ++ ".\\, " ++ toLatexRulesTerm term ++ ")"
-}

displayOrdinaryPred pred info interms outterms = ("\\mathrm{" ++ (pred ++ (firstCharUP (concat info))) ++ "}") ++ "\\," ++ (intercalate sep (map toLatexRulesTerm interms)) ++ sep ++ (intercalate sep (map toLatexRulesTerm outterms))
displayOrdinaryTypeOf pred info interms outterms = "\\Gamma " ++ pred ++ "\\," ++ (intercalate sep (map toLatexRulesTerm interms)) ++ ":" ++ (intercalate sep (map toLatexRulesTerm outterms))
displayBinaryInfixIO pred info interms outterms = (toLatexRulesTerm (head interms)) ++ pred ++ (toLatexRulesTerm (head outterms))
displayBinaryInfixII pred info interms outterms = (toLatexRulesTerm (head interms)) ++ pred ++ (toLatexRulesTerm (interms !! 1))

symbols :: [(String, ([String] -> [Term] -> [Term] -> String))]
symbols = 
     [(typeOf, displayOrdinaryTypeOf "\\vdash "),
     (typeOfGradual, displayOrdinaryTypeOf "\\typeOfG "),
     (typeOfCC, displayOrdinaryTypeOf "\\typeOfCC "),
     (typeOfCI, \info -> \interms -> \outterms -> "\\Gamma \\typeOfCI " ++ (intercalate sep (map toLatexRulesTerm interms)) ++ "\\hookrightarrow " ++ (toLatexRulesTerm (outterms !! 0)) ++ ":" ++ (toLatexRulesTerm (outterms !! 1))),
     ("step", displayBinaryInfixIO "\\leadsto "),
     ("flow", displayBinaryInfixII "\\sim "),
     ("=", displayBinaryInfixII " = "),
     ("match", \info -> \interms -> \outterms -> (toLatexRulesTerm (head interms)) ++ "\\mathrel{\\triangleright} " ++ (toLatexRulesTerm (Constructor (concat info) outterms))),
     ("consistency", displayBinaryInfixII "\\approx "),
     ("subtype", displayBinaryInfixII "<:"),
     ("subtypeG", \info -> \interms -> \outterms -> (toLatexRulesTerm (outterms !! 0)) ++ "\\vdash " ++ (displayBinaryInfixII "\\lesssim " info interms outterms))
     ]

getTerm :: Rule -> Term
getTerm (Rule premises conclusion) = getFirstInputOfPremise conclusion
--getTerm rule = error (show rule)

getType :: Rule -> Term
getType (Rule premises conclusion) = getFirstOutputOfPremise conclusion

generateCanonical :: SignatureEntry -> Term
generateCanonical (Decl c typ contraInfo contexts entries) = (Constructor c (map symbolByEntry entries)) where symbolByEntry = \entry -> case entry of {Simple "typ" -> Var ("T") ; Simple "term" -> Var ("E") ; Simple "label" -> Var ("L") ; Abs something "typ" -> Var ("T") ; Abs something "term" -> Var ("E") ; other -> error (show "ERROR: generateCanonical") }

suitable_transformation :: String -> String
suitable_transformation variable = let chop = (last variable) in let butLast = (take ((length variable) -1) variable) in if [chop] == "'" then (suitable_transformation butLast) ++ "'" else if isDigit chop then (suitable_transformation butLast) ++ "_" ++ [chop] else if startswith "V" variable || startswith "E" variable || startswith "L" variable then (map toLower variable) else variable 

makeSection :: String -> String
makeSection title = "\\section{" ++ title ++ "}\n"
makeSubSection title = "\\subsection{" ++ title ++ "}\n"

axioms :: [(String, String)]
axioms = [
      (typeOf, " { x:T, \\Gamma \\vdash x:T } "),
      (typeOfGradual, " { x:T, \\Gamma \\typeOfG x:T } "),
      (typeOfCC, " { x:T, \\Gamma \\typeOfCC x:T } "),
      (typeOfCI, " { x:T, \\Gamma \\typeOfCI x \\hookrightarrow x :T } ")
      ]

gradualTitle :: String -> String
gradualTitle title = if startswith "simply" (map toLower title) then replaceSimply else if startswith "stlc" (map toLower title) then replaceSTLC else "The Gradually Typed Language for " ++ title where
             replaceSimply = "Gradually Typed Lambda Calculus (GTLC)" -- ++ (drop (length "simply") title) 
             replaceSTLC = "GTLC" ++ (drop (length "STLC") title) 

castTitle :: String -> String
castTitle title = "The Cast Calculus for " ++ (gradualTitle title)