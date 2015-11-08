module FinalType where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching
import InsertJoin

-- Intial flow premises trumps the methodology.
-- Checking first flow premises is necessary for cast insertion, or new variables will 
finalType :: Package -> Term -> Term
finalType pkg (Var variable) = if userType pkg (Var variable) || consumer pkg (Var variable) || (not (multipleOutputs pkg (Var variable))) then (Var variable) else makeJoinVar (Var variable) 
finalType pkg other = error ((show other) ++ (show pkg))

destinationType :: Package -> Term -> Term
destinationType pkg (Var variable) = case flowTermByVar (pkg_getRule pkg) (Var variable) of 
                                        Nothing -> checkIfPatternMatched
                                        Just anotherVar -> anotherVar
                                      where 
                                      checkIfPatternMatched = case patternMatchByVar (pkg_getRule pkg) (Var variable) of
                                        Nothing -> (Var variable)
                                        Just premise -> destinationType pkg (reverseMatch pkg premise) 
destinationType pkg (Constructor c terms) = (Constructor c (map (destinationType pkg) terms)) 


flowTermByVar :: Rule -> Term -> Maybe Term
flowTermByVar rule term = case searchPremiseByPredAndVar rule "flow" term of { Nothing -> Nothing ; Just (Formula pred strings interms outterms) -> Just (interms !! 1) }

-- consumer is called after the pattern-matching step. Consumers are now only inside a pmatch premise.
consumer :: Package -> Term -> Bool
consumer pkg (Var variable) = case (pkg_getRule pkg) of (Rule premises conclusion) -> any (consumer_premise pkg (Var variable)) premises || consumer_premise pkg (Var variable) conclusion
consumer_premise :: Package -> Term -> Premise -> Bool
consumer_premise pkg (Var variable) (Formula pred strings interms outterms) = if pred == "match" then case elemIndex (Var variable) outterms of {Nothing -> False ; Just n -> n+1 <= (contravariantArguments pkg (head strings))} else False
consumer_premise pkg (Var variable) (Hypothetical premise1 premise2) = False

userType :: Package -> Term -> Bool 
userType pkg variable = case (pkg_getRule pkg) of (Rule premises conclusion) -> case conclusion of (Formula pred strings interms outterms) -> elem variable (concat (map varsOf interms))

multipleOutputs :: Package -> Term -> Bool
multipleOutputs pkg var = (length (elemIndices var (outputsOrInputs "outputs" (pkg_getRule pkg)))) > 1 

outputsOrInputs :: String -> Rule -> [Term]
outputsOrInputs io (Rule premises conclusion) = concat (map (outputsList_prem io) premises) ++ outputsList_prem (inverse io) conclusion where inverse = \s -> if s == "inputs" then "outputs" else "inputs"
outputsList_prem :: String -> Premise -> [Term]
outputsList_prem io (Formula pred strings interms outterms) = if io == "inputs" then (concat (map varsOf interms)) else (concat (map varsOf outterms))
outputsList_prem io (Hypothetical premise1 premise2) = if io == "inputs" then outputsList_prem "outputs" premise1 else outputsList_prem "outputs" premise2

{- 
destinationType :: Package -> Term -> Term
destinationType pkg (Var variable) = case flowTermByVar (pkg_getRule pkg) (Var variable) of 
                                        Nothing -> checkIfPatternMatched
                                        Just anotherVar -> anotherVar
                                      where 
                                      checkIfPatternMatched = case patternMatchByVar (pkg_getRule pkg) (Var variable) of
                                        Nothing -> (Var variable)
                                        Just premise -> case reverseMatch pkg premise of { (Constructor c inputs outputs) -> (Constructor c (map (destinationType pkg) inputs) (map (destinationType pkg) outputs)) ; otherwise -> error (show otherwise) }
destinationType pkg other = error (show other)
-}
