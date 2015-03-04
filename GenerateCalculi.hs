module GenerateCalculi where

import System.IO.Unsafe 
import Data.Unique
import TypeSystem
import PatternMatching
import UpToConsistency
import ResolutionAndStatic
import TypeSystemForCC
import CastInsertion
import ToLambdaProlog
import GenerateAuxiliaryPredicates


generateComplete :: TypeSystem -> IO ()
generateComplete ts = do 
						putStrLn ""
						(toLambdaPrologIO "Original" ts)
						(toLambdaPrologIO "Gradual" ts)
						(toLambdaPrologIO "CC" ts)
						(toLambdaPrologIO "CI" ts)
						(matchesRulesIO "Gr" ts)
						(joinRulesIO ts)
						
sig_stlc :: [SignatureEntry]
sig_stlc = [Decl "arrow" "typ" [(Simple "typ"),(Simple "typ")]] 

absR :: Rule
absR = (Rule 
			[(Hypothetical "x" (Bound "x") (Var "T1") (Application (Var "R") (Bound "x")) (Var "T2"))] 
			(Constructor "abs" [(Var "T1")] [(Var "R")]) 
			(Constructor "arrow" [(Var "T1")] [(Var "T2")]))

app :: Rule
app = (Rule 
			[(Formula "typeOf" [] [(Var "E1")] [(Constructor "arrow" [(Var "T1")] [(Var "T2")])]), 
			(Formula "typeOf" [] [(Var "E2")] [(Var "T1")])] 
		(Constructor "app" [] [(Var "E1"), (Var "E2")]) 
		(Var "T2"))

stlc :: TypeSystem
stlc = (Ts sig_stlc [absR, app])


appNested :: Rule
appNested = (Rule 
		[(Formula "typeOf" [] [(Var "E1")] [(Constructor "arrow" [(Constructor "times" [] [(Var "T1"), (Var "T2")])]   [(Constructor "times" [] [(Var "T3"), (Var "T4")])])]), 
		(Formula "typeOf" [] [(Var "E2")] [(Var "T1")])] 
			(Constructor "app" [] [(Var "E1"), (Var "E2")]) 
			(Var "T2"))

stlcNest :: TypeSystem
stlcNest = (Ts sig_stlc [absR, appNested])


sig_unit :: [SignatureEntry]
sig_unit = [Decl "unitType" "typ" []]

unit :: Rule
unit = (Rule [] (Var "unit") (Constructor "unitType" [] []))

stlc_unit = extendTypeSystem stlc sig_unit [unit]

sig_let :: [SignatureEntry]
sig_let = [Decl "let" "term" [(Simple "term"),(Abs "term" "term")]]

letR :: Rule
letR = (Rule 
		[(Formula "typeOf" [] [(Var "E")] [(Var "T1")]), 
		(Hypothetical "x" (Bound "x") (Var "T1") (Application (Var "R") (Bound "x")) (Var "T2"))] 
			(Constructor "let"  [] [(Var "E"), 
			(Var "R")]) (Var "T2")) 

stlc_letR = extendTypeSystem stlc sig_let [letR]


pairType :: String
pairType = "pairType"

sig_pairs :: [SignatureEntry]
sig_pairs = [Decl pairType "typ" [(Simple "typ"),(Simple "typ")]] 


makepair :: Rule
makepair = (Rule 
		[(Formula "typeOf" [] [(Var "E1")] [(Var "T1")]), 
		(Formula "typeOf" [] [(Var "E2")] [(Var "T2")])] 
			(Constructor "pair" [] [(Var "E1"), (Var "E2")]) 
			(Constructor pairType [] [(Var "T1"), (Var "T2")]))

proj1 :: Rule
proj1 = (Rule [(Formula "typeOf" [] [(Var "E")] [(Constructor pairType [] [(Var "T1"), (Var "T2")])])] (Constructor "fst" [] [(Var "E")]) (Var "T1"))

proj2 :: Rule
proj2 = (Rule [(Formula "typeOf" [] [(Var "E")] [(Constructor pairType [] [(Var "T1"), (Var "T2")])])] (Constructor "snd" [] [(Var "E")]) (Var "T2"))

stlc_pairs = extendTypeSystem stlc sig_pairs [makepair, proj1, proj2]


sumType :: String
sumType = "plus"

sig_sum :: [SignatureEntry]
sig_sum = [Decl sumType "typ" [(Simple "typ"),(Simple "typ")]] 


caseR :: Rule 
caseR = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Constructor sumType [] [(Var "T1"), (Var "T2")])]),
				(Hypothetical "x" (Bound "x") (Var "T1") (Application (Var "R1") (Bound "x")) (Var "T")),
				(Hypothetical "x" (Bound "x") (Var "T2") (Application (Var "R2") (Bound "x")) (Var "T"))
				] (Constructor "case"  [] [(Var "E"), (Var "R1"), (Var "R2")]) (Var "T"))

inl :: Rule
inl = (Rule 	
		[(Formula "typeOf" [] [(Var "E")] [(Var "T1")])] 
			(Constructor "inl"  [] [(Var "T2"), (Var "E")]) 
			(Constructor sumType [] [(Var "T1"), (Var "T2")]))

inr :: Rule
inr = (Rule 	
		[(Formula "typeOf" [] [(Var "E")] [(Var "T2")])] 
			(Constructor "inr" [] [(Var "T1"), (Var "E")]) 
			(Constructor sumType [] [(Var "T1"), (Var "T2")]))

{- inl and inr should also take a type, to fix -}

stlc_sum = extendTypeSystem stlc sig_sum [caseR, inl, inr]


sig_fix :: [SignatureEntry]
sig_fix = [Decl "fix" "term" [(Simple "term"),(Simple "term")]]

fixR :: Rule
fixR = (Rule	
		[(Formula "typeOf" [] [(Var "E")] [(Constructor "arrow" [(Var "T")] [(Var "T")])])
		] 
			(Constructor "fix" [] [(Var "E")]) 
			(Var "T"))

stlc_fix = extendTypeSystem stlc sig_fix [fixR]


sig_list :: [SignatureEntry]
sig_list = [(Decl "list" "typ" [(Simple "typ")]),
			(Decl "emptyList" "term" [(Simple "type")]),
			(Decl "isnil" "term" [(Simple "type"), (Simple "term")]),
			(Decl "consR" "term" [(Simple "type"), (Simple "term"), (Simple "term")]),
			(Decl "headR" "term" [(Simple "type"), (Simple "term")]),
			(Decl "tailR" "term" [(Simple "type"), (Simple "term")])
			] 

nilR :: Rule
nilR = (Rule 
		[] 
			(Constructor "emptyList" [] [(Var "T")]) 
			(Constructor "list" [] [(Var "T")]))

isnil :: Rule
isnil = (Rule 	
		[(Formula "typeOf" [] [(Var "E")] [(Constructor "list" [] [(Var "T")])])] 
			(Constructor "isnil" [] [(Var "T"), (Var "E")]) 
			(Var "Bool"))

consR :: Rule
consR = (Rule 	
		[(Formula "typeOf" [] [(Var "E1")] [(Var "T")]),
		(Formula "typeOf" [] [(Var "E2")] [(Constructor "list" [] [(Var "T")])])] 
			(Constructor "cons" [] [(Var "T"), (Var "E1"), (Var "E2")]) 
			(Constructor "list" [] [(Var "T")]))

headR :: Rule 
headR = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Constructor "list" [] [(Var "T")])])
				] (Constructor "head" [] [(Var "T"), (Var "E")]) (Var "T"))

tailR :: Rule 
tailR = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Constructor "list" [] [(Var "T")])])
				] (Constructor "tail" [] [(Var "T"), (Var "E")]) (Var "T"))

stlc_lists = extendTypeSystem stlc sig_list [nilR, isnil, consR, headR, tailR]



sig_ref :: [SignatureEntry]
sig_ref = [(Decl "refType" "typ" [(Simple "typ")]),
			(Decl "ref" "term" [(Simple "term")]),
			(Decl "deref" "term" [(Simple "term")]),
			(Decl "assign" "term" [(Simple "term"), (Simple "term")])
			] 

ref :: Rule
ref = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Var "T")])
				] 
					(Constructor "ref" [] [(Var "E")]) 
					(Constructor "refType" [(Var "T")] []))

deref :: Rule
deref = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Constructor "refType" [(Var "T")] [])])
				] 
					(Constructor "deref" [] [(Var "E")]) 
					(Var "T"))

assign = (Rule 	[
				(Formula "typeOf" [] [(Var "E1")] [(Constructor "refType" [(Var "T")] [])]),
				(Formula "typeOf" [] [(Var "E2")] [(Var "T")])
				] (Constructor "assign" [] [(Var "E1"), (Var "E2")]) (Constructor "unitType" [] []))

stlc_ref = extendTypeSystem stlc sig_ref [ref, deref, assign]


sig_exc :: [SignatureEntry]
sig_exc = [(Decl "excType" "typ" [(Simple "typ")]),
			(Decl "raise" "typ" [(Simple "typ"), (Simple "term")]),
			(Decl "try" "typ" [(Simple "term"), (Simple "term")])
			] 

raiseR :: Rule
raiseR = (Rule 	[
				(Formula "typeOf" [] [(Var "E")] [(Constructor "excType" [] [])])
				] 
					(Constructor "raise" [] [(Var "T"), (Var "E")]) 
					(Var "T"))

tryR :: Rule
tryR = (Rule 	[
				(Formula "typeOf" [] [(Var "E1")] [(Var "T")]),
				(Formula "typeOf" [] [(Var "E2")] [(Constructor "arrow" [(Constructor "excType" [] [])] [(Var "T")])])
				] 
					(Constructor "try" [] [(Var "E1"), (Var "E2")]) 
					(Var "T"))

stlc_exc = extendTypeSystem stlc sig_exc [raiseR, tryR]



{- Missing, to do: tuples, records -}
{- Missing, not to do: universal, existential -}

