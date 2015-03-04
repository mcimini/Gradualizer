module ResolutionAndStatic where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching 
import UpToConsistency 

suffixGr :: String
suffixGr = "Gr"

toGradual :: TypeSystem -> TypeSystem
toGradual ts = (Ts sig rules') 
				where
					((Ts sig rulesUP), equatedByRules) = (upToConsistencyEqualities (patternMatches ts))
					rules1 = insertJoinAndConsistency equatedByRules rulesUP
					rules2 = zipWith toGradualR rules1 equatedByRules
					rules3 = zipWith addStaticPremisesR rules2 equatedByRules
					rules' = map removeUselessConsistencyR rules3
					addStaticPremisesR = \r -> \equated -> case r of (Rule premises term typ) -> (Rule (concat (map (addStaticPremisesPr equated) premises)) term typ)
					removeUselessConsistencyR = \r -> case r of (Rule premises term typ) -> (Rule (removeUselessConsistencyPr premises) term typ)

addStaticPremisesPr :: TrackOfVars -> Premise -> [Premise]
addStaticPremisesPr equated (Formula pred strings interms outterms) = [(Formula pred strings interms outterms)] ++ (concat (map (addStaticPremisesTrm equated) interms))
addStaticPremisesPr equated (Hypothetical bound outterm1 interm1 interm2 outterm2) = [(Hypothetical bound outterm1 interm1 interm2 outterm2)] ++ (addStaticPremisesTrm equated interm1)

addStaticPremisesTrm :: TrackOfVars -> Term -> [Premise]
addStaticPremisesTrm equated (Var variable) = if (isType (Var variable)) then let tmp = (HM.lookup (Var variable) equated) in
													case tmp of
														Just values -> []
														Nothing -> [(Formula "static" [] [(Var variable)] [])]
												else []
addStaticPremisesTrm equated (Constructor c interms outterms) = (concat (map (addStaticPremisesTrm equated) interms)) ++ (concat (map (addStaticPremisesTrm equated) outterms))
addStaticPremisesTrm equated (Bound var) = []
addStaticPremisesTrm equated (Application term1 term2) = (addStaticPremisesTrm equated term1) ++ (addStaticPremisesTrm equated term2)


removeUselessConsistencyPr :: [Premise] -> [Premise]
removeUselessConsistencyPr [] = []
removeUselessConsistencyPr (premise:rest) = case premise of
								(Formula pred strings interms outterms) -> if (pred == "consistency" && (isType source)) then (removeUselessConsistencyPr rest) else premise:(removeUselessConsistencyPr rest)
									where 
									source = (head outterms)
									target = (last outterms)
								(Hypothetical bound outterm1 interm1 interm2 outterm2) -> premise:(removeUselessConsistencyPr rest)

toGradualR :: Rule -> TrackOfVars -> Rule
toGradualR (Rule premises term typ) outputs = (Rule (map (toGradualPr outputs) premises) term (toGradualTrOne outputs typ))

toGradualPr :: TrackOfVars -> Premise -> Premise
toGradualPr outputs (Formula pred strings interms outterms) = if (not (isUserPredicate pred)) then (Formula pred strings interms outterms) else (Formula pred strings (map (toGradualTrOne outputs) interms) outterms)
toGradualPr outputs (Hypothetical bound outterm1 interm1 interm2 outterm2) = (Hypothetical bound outterm1 (toGradualTrOne outputs interm1) (toGradualTrOne outputs interm2) outterm2)

toGradualTrOne :: TrackOfVars -> Term -> Term
toGradualTrOne outputs (Var variable) = let tmp = (HM.lookup (Var variable) outputs) in
											case tmp of 
												Just values -> let tmp2 = (HM.lookup (Var "TYPEINPUTS") outputs) in 
													(let replacement = if (length values == 2) then (last values) else (Var ("Join" ++ (toStringT (Var variable)))) in 
														case tmp2 of 
														Just values -> if (elem (Var variable) values) then (Var variable) else replacement
														Nothing -> replacement)
										  		Nothing -> (Var variable)
toGradualTrOne outputs (Constructor c interms outterms) = (Constructor c (map (toGradualTrOne outputs) interms) (map (toGradualTrOne outputs) outterms))
toGradualTrOne outputs (Bound var) = (Bound var)
toGradualTrOne outputs (Application term1 term2) = (Application (toGradualTrOne outputs term1) (toGradualTrOne outputs term2))

