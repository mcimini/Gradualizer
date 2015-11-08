module ToLambdaProlog where

import Data.List
import Data.Char
import TypeSystem
import PatternMatching
import FlowDiscovery
import Static
import TypeSystemForCC
import CastInsertion
import GenerateAuxiliaryPredicates
import CastManagement

toLambdaPrologSig :: String -> Signature -> String
toLambdaPrologSig mode sig = if mode == "Gradual" then (concat (map toLambdaPrologSig_entry sig)) else "kind label\t type.\n\n" ++ (concat (map toLambdaPrologSig_entry sig))

toLambdaPrologSig_entry :: SignatureEntry -> String
toLambdaPrologSig_entry (Decl c typ contraInfo contexts entries) = "type " ++ c ++ "\t\t" ++ (intercalate " -> " ((map toLambdaPrologSig_TE entries) ++ [typ])) ++ ".\n"

toLambdaPrologSig_TE :: TypeEntry -> String
toLambdaPrologSig_TE (Simple str) = str
toLambdaPrologSig_TE (Abs str1 str2) = "(" ++ str1 ++ " -> " ++ str2 ++ ")"


toLambdaPrologModule :: TypeSystem -> String
toLambdaPrologModule (Ts sig rules) = unlines (map toLambdaPrologR rules)
						
toLambdaPrologR :: Rule -> String 
toLambdaPrologR (Rule premises conclusion) = 
			toLambdaPrologPr conclusion ++ premisesIfAny ++ "."
			where 
			premisesIfAny = if (premisesWithComma == "") then "" else  " :- " ++ premisesWithComma
			premises' = (map toLambdaPrologPr premises)
			premisesWithComma = intercalate ", " premises'
			 
toLambdaPrologPr :: Premise -> String
toLambdaPrologPr (Formula pred info interms outterms) = if pred == "=" then (intercalate " = " (map toLambdaPrologTerm interms)) else (toLambdaPrologPredicateName pred info) ++ displayInputs ++ displayOutputs
                 where
                 displayInputs = if calculatedIn == "" then "" else " " ++ calculatedIn
                 displayOutputs = if calculatedOut == "" then "" else " " ++ calculatedOut
                 calculatedIn = (intercalate " " (map toLambdaPrologTerm interms)) 
                 calculatedOut = (intercalate " " (map toLambdaPrologTerm outterms)) 
												
toLambdaPrologPr (Hypothetical premise1 premise2) = "(pi x\\ (" ++ (toLambdaPrologPr premise1) ++ " => " ++ (toLambdaPrologPr premise2) ++ "))"
toLambdaPrologPr (Negated premise) = "not (" ++ (toLambdaPrologPr premise) ++ ")"

toLambdaPrologTerm :: Term -> String
toLambdaPrologTerm (Var varterm) = varterm
toLambdaPrologTerm (Constructor c terms) = "(" ++ c ++ display ++ ")"
													where 
													display = if calculated == "" then "" else " " ++ calculated
													calculated = (intercalate " " (map toLambdaPrologTerm terms)) 

toLambdaPrologTerm (Application term1 term2) = "(" ++ toLambdaPrologTerm term1 ++ " " ++ toLambdaPrologTerm term2 ++ ")"
toLambdaPrologTerm (Bound x) = x
toLambdaPrologTerm (Lambda boundvar term) = "(" ++ boundvar ++ "\\ " ++ toLambdaPrologTerm term ++ ")"

toLambdaPrologPredicateName :: String -> [String] -> String
toLambdaPrologPredicateName pred info = case pred of 
	"subtypeG" -> pred
	otherwise -> pred ++ (firstCharUP (concat info))

