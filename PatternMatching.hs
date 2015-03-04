module PatternMatching where

import System.IO.Unsafe 
import Data.Unique
import TypeSystem


gensymPM :: IO String
gensymPM = do 
			sym <- newUnique
			return (pmTypePrefix ++ show (hashUnique sym))
			
patternMatches :: TypeSystem -> TypeSystem
patternMatches (Ts sig rules) = (Ts sig (map patternMatchesR rules))

patternMatchesR :: Rule -> Rule 
patternMatchesR (Rule premises term typ) = (Rule (concat (map patternMatchesPr premises)) term typ) 
{-											where 
												([term'], fromConclusion) = patternMatchesTrms [term]
-}	

patternMatchesPr :: Premise -> [Premise]
patternMatchesPr (Formula pred strings terms types) = (Formula pred strings terms types'):pmPremises
									where 
									(types', pmPremises) = (patternMatchesTrms types)
patternMatchesPr (Hypothetical bound term1 type1 term2 type2) = (Hypothetical bound term1 type1' term2 type2'):pmPremises
									where 
									type1' = head type11
									type2' = head type22
									(type11, pmPremises1) = (patternMatchesTrms [type1])
									(type22, pmPremises2) = (patternMatchesTrms [type2])
									pmPremises = pmPremises1 ++ pmPremises2 

patternMatchesTrms :: [Term] -> ([Term], [Premise])
patternMatchesTrms [] = ([],[])
patternMatchesTrms (typ:rest) = let (typesRest, pmPremisesRest) = (patternMatchesTrms rest) in
								 (case typ of 
									(Var var) -> ((Var var):typesRest, pmPremisesRest)
									(Constructor c inputs outputs) -> 
										let (inputsInner, inpPremisesInner) = (patternMatchesTrms inputs) in 
										let (outputsInner, outPremisesInner) = (patternMatchesTrms outputs) in
										((Var freshVar):typesRest, ((patternMatchesPr (Formula "match" [c] ((Var freshVar):inputsInner)  outputsInner)) ++ inpPremisesInner ++ outPremisesInner ++ pmPremisesRest)))
							where 
								freshVar = (unsafePerformIO gensymPM)

{-

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

absITLC :: Rule
absITLC = (Rule 
			[(Hypothetical "x" (Bound "x") (Var "T1") (Application (Var "R") (Bound "x")) (Var "T2"))] 
			(Constructor "abs" [] [(Var "R")]) 
			(Constructor "arrow" [(Var "T1")] [(Var "T2")]))

itlc :: TypeSystem
itlc = (Ts sig_stlc [absITLC, app])
			



app :: Rule
app = (Rule [(Formula "typeOf" [] [(Var "E1")] 
			[ (Constructor "arrow" 
			    [
					(Constructor "times" [(Var "T1"), (Var "T2")]), 
					(Constructor "times" [(Var "T3"), (Var "T4")])
				])]), 
			(Formula "typeOf" [] [(Var "E2")] [(Var "T1")])] (Constructor "app" [(Var "E1"), (Var "E2")]) (Var "T1"))

-}