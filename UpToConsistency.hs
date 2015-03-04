module UpToConsistency where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching 

type TrackOfVars = (HM.Map Term [Term])

{- Instead of generating a type sytem with binary equalities, an hashmap TrackOfVars will keep track of the equated types. 
-}

gensymTT :: IO String
gensymTT = do 
			sym <- newUnique
			return (outputTypePrefix ++ show (hashUnique sym))

upToConsistency :: TypeSystem -> TypeSystem
upToConsistency ts = (Ts sig rules')
				where
					((Ts sig rules), equatedByRules) = (upToConsistencyEqualities (patternMatches ts))
					rules' = insertJoinAndConsistency equatedByRules rules

insertJoinAndConsistency :: [TrackOfVars] -> [Rule] -> [Rule] 
insertJoinAndConsistency equatedByRules rules = zipWith insertJoin rules newPremises
									where
									newPremises = map equalityPremise equatedByRules
									insertJoin = (\x -> \y -> case x of 
											(Rule premises term typ) -> (Rule (premises ++ y) term typ)) 

		
equalityPremise :: TrackOfVars -> [Premise]
equalityPremise equated = concat (map snd (HM.toList addedPremises)) where
						addedPremises = HM.map (wrappingEquation equated) (HM.delete (Var "TYPEINPUTS") equated)


wrappingEquation :: TrackOfVars -> [Term] -> [Premise]
wrappingEquation equated types = join ++ consistency
					where
					consistency = if (length typesMinusInput) > 0 then (let tmp = (HM.lookup (Var "TYPEINPUTS") equated) in
									(case tmp of 
										Just values -> if (elem (head types) values) then [(Formula "consistency" [] [] [flowType, (head types)])] else [(Formula "consistency" [] [] [(head types), flowType])]
										Nothing -> []))
								 else []
					join = if (length typesMinusInput) > 1 then [(Formula "join" [(show (length typesMinusInput))] typesMinusInput [joinType])] else []
					typesMinusInput = tail types
					joinType = (Var ("Join" ++ (toStringT (head types))))
					flowType = if (length typesMinusInput == 1) then (head typesMinusInput) else joinType

upToConsistencyEqualities :: TypeSystem -> (TypeSystem, [TrackOfVars])
upToConsistencyEqualities (Ts sig rules) = ((Ts sig rules'), outputs)
						where
							setOfPairs = (map upToConsistencyR rules)
							rules' = map fst setOfPairs
							outputs = map snd setOfPairs

upToConsistencyR :: Rule -> (Rule , TrackOfVars)
upToConsistencyR (Rule premises term typ) = ((Rule (premises') term' typ) , outputs)
									where 
										(premises', outputs1) = upToConsistencyPr premises HM.empty 
										(term', outputs) = upToConsistencyTrmOne term outputs1 

										
upToConsistencyPr :: [Premise] -> TrackOfVars -> ([Premise], TrackOfVars)
upToConsistencyPr [] outputs =  ([], outputs)
upToConsistencyPr (premise:rest) outputs =  (premise':rest', outputs')
									where
										(premise', outputs1) = upToConsistencyPrOne premise outputs 
										(rest', outputs') = upToConsistencyPr rest outputs1 

upToConsistencyPrOne :: Premise -> TrackOfVars -> (Premise, TrackOfVars)
upToConsistencyPrOne (Formula pred strings interms outterms) outputs = ((Formula pred strings interms outterms'), outputs')
									where 
										(outterms', outputs1) = upToConsistencyTrm outterms outputs
										outputs' = if (pred == "match") && ((length interms) >= 2) then (trackOfTypeInputs outputs1 (drop 1 interms)) else outputs1

upToConsistencyPrOne (Hypothetical bound outterm1 interm1 interm2 outterm2) outputs = ((Hypothetical bound outterm1 interm1 interm2 outterm2'), outputs')
									where 
										(outterm2', outputs') = upToConsistencyTrmOne outterm2 outputs
									
trackOfTypeInputs :: TrackOfVars -> [Term] -> TrackOfVars
trackOfTypeInputs equated typeinputs = insertTypeInputs inputsRecorded typeinputs
										where inputsRecorded = let tmp = (HM.lookup (Var "TYPEINPUTS") equated) in
												case tmp of 
													Just values -> (HM.adjust (++ typeinputs) (Var "TYPEINPUTS") equated)
													Nothing -> (HM.insert (Var "TYPEINPUTS") typeinputs equated)
														
insertTypeInputs :: TrackOfVars -> [Term] -> TrackOfVars
insertTypeInputs outputs [] = outputs
insertTypeInputs outputs (typ:rest) = (insertTypeInputs outputs' rest)
									where outputs' = (let tmp = (HM.lookup typ outputs) in
														(case tmp of 
															Just values -> outputs
										  					Nothing -> (HM.insert typ [typ] outputs)))


upToConsistencyTrm :: [Term] -> TrackOfVars -> ([Term], TrackOfVars)
upToConsistencyTrm [] outputs = ([], outputs)
upToConsistencyTrm (term:rest) outputs = (term':rest', outputs')
									where
										(term', outputs1) = upToConsistencyTrmOne term outputs
										(rest', outputs') = upToConsistencyTrm rest outputs1

upToConsistencyTrmOne :: Term -> TrackOfVars -> (Term, TrackOfVars)
upToConsistencyTrmOne (Var variable) outputs = if (isType (Var variable)) then (let tmp = (HM.lookup (Var variable) outputs) in
													(case tmp of 
														Just values -> ((Var freshVar), (HM.adjust (++ [(Var freshVar)]) (Var variable) outputs))
										  				Nothing -> ((Var freshVar), (HM.insert (Var variable) [(Var variable),(Var freshVar)] outputs))))											
												else ((Var variable), outputs)
												where 
													freshVar = (unsafePerformIO gensymTT)
upToConsistencyTrmOne (Constructor c inputsCapab outputsCapab) outputs = ((Constructor c inputsCapab outputsCapab'), outputs')
												where 
												(outputsCapab', outputs1) = upToConsistencyTrm outputsCapab outputs
												outputs' = (trackOfTypeInputs outputs1 inputsCapab) 
upToConsistencyTrmOne (Bound var) outputs = ((Bound var), outputs)
upToConsistencyTrmOne (Application term1 term2) outputs = ((Application term1 term2), outputs)
upToConsistencyTrmOne term outputs = error "Error: Unexpected form in output position: Encode"


{-						
upToConsistencyTrmOne (Application term1 term2) outputs = ((Application term1' term2'), outputs')
											where 
												(term1', outputs1) = upToConsistencyTrmOne term1 outputs
												(term2', outputs') = upToConsistencyTrmOne term1 outputs1

												((Constructor c inputsCapab' outputsCapab'), outputs')
	
where 
outputs1 = HM.fold HM.union (map (trackOfTypeInputs outputs) inputsCapab)
(outputsCapab', outputs') = upToConsistencyTrm outputsCapab outputs1


-}