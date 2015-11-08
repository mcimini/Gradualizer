module FlowDiscovery where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching
import FinalType
import InsertJoin

outputTypePrefix :: String
outputTypePrefix = "New"

flowDiscovery :: TypeSystem -> TypeSystem
flowDiscovery ts = (Ts sig rules')
              where
              (Ts sig rules) = (tmpFlowDiscovery (patternMatches ts))
              rules' = map insertJoin rules

tmpFlowDiscovery :: TypeSystem -> TypeSystem
tmpFlowDiscovery (Ts sig rules) = (Ts sig (map (tmpFlowDiscovery_rule sig) rules))

-- Notice that we have to propagate the rule that is examined, for the calculation of the final type. 
tmpFlowDiscovery_rule :: Signature -> Rule -> Rule
tmpFlowDiscovery_rule sig (Rule premises conclusion) = (Rule (fst (tmpFlowDiscovery_prems pkg 1 premises)) (inputsToFinalType pkg conclusion)) where pkg = (sig, (Rule premises conclusion))

tmpFlowDiscovery_prems :: Package -> Int -> [Premise] -> ([Premise], Int)
tmpFlowDiscovery_prems pkg n [] = ([], n)
tmpFlowDiscovery_prems pkg n ((Formula pred strings interms outterms):rest) = if pred == "subtype" then subtypeTreatment interms n pkg rest else ((Formula pred strings interms outterms'):(filter onlyRelevantFlows flows) ++ newPremises, z)
                      where
                      (flows, m) = tmpFlowDiscovery_term pkg n outtermsButNoConsumers
                      (newPremises, z) = tmpFlowDiscovery_prems pkg m rest
                      (outtermsButNoConsumers, outterms') = if pred == "match" && isContravariant pkg (head strings) then ((drop (contravariantArguments pkg (head strings)) outterms), (take (contravariantArguments pkg (head strings)) outterms) ++  (map theNewVarsOf flows)) else (outterms, (map theNewVarsOf flows))
tmpFlowDiscovery_prems pkg n ((Hypothetical premise1 premise2):rest) = ((Hypothetical (inputsToFinalType pkg premise1) (head flows)):(filter onlyRelevantFlows flows) ++ newPremises, z)
                      where
                      (flows, m) = tmpFlowDiscovery_prems pkg n [premise2]
                      (newPremises, z) = tmpFlowDiscovery_prems pkg m rest

tmpFlowDiscovery_term :: Package -> Int -> [Term] -> ([Premise], Int)
tmpFlowDiscovery_term pkg n [] = ([],n)
tmpFlowDiscovery_term pkg n ((Var variable):rest) = if multipleOutputs pkg (Var variable) then let (newrest, z) = tmpFlowDiscovery_term pkg (n+1) rest in ((Formula "flow" [] [(Var freshvar), finalType pkg (Var variable)] []):newrest, z) else let (newrest, z) = tmpFlowDiscovery_term pkg n rest in (((Formula "flow" [] [(Var variable), (Var variable)] []):newrest), z) where freshvar = (outputTypePrefix ++ (show n)) -- (unsafePerformIO gensymTT)
tmpFlowDiscovery_term pkg n other = error "Found a term that is not a variable."

theNewVarsOf :: Premise -> Term
theNewVarsOf (Formula pred strings interms outterms) = if pred == "flow" then head interms else error "theNewVarsOf called with a premise that is not a flow premise"

onlyRelevantFlows :: Premise -> Bool
onlyRelevantFlows (Formula pred strings interms outterms) = if pred == "flow" then not (interms !! 0 == interms !! 1) else False

inputsToFinalType :: Package -> Premise -> Premise
inputsToFinalType pkg (Formula pred strings interms outterms) = (Formula pred strings interms (map (inputsToFinalType_term pkg) outterms))

inputsToFinalType_term :: Package -> Term -> Term
inputsToFinalType_term pkg (Var variable) = finalType pkg (Var variable)
inputsToFinalType_term pkg (Constructor c terms) = (Constructor c (map (finalType pkg) terms))

makeFlow :: Term -> Term -> Premise
makeFlow = \type1 -> \type2 -> Formula "flow" [] [type1, type2] []

subtypeTreatment :: [Term] -> Int -> Package -> [Premise] -> ([Premise], Int)
subtypeTreatment arguments n pkg rest = ((Formula "subtypeG" [freshvar] arguments []):newPremises, z)
	where 
	freshvar = (outputTypePrefix ++ (show n))
	(newPremises, z) = tmpFlowDiscovery_prems pkg (n+1) rest
	

