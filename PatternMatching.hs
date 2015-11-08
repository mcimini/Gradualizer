module PatternMatching where

import Data.Unique
import Data.List
import TypeSystem


pmTypePrefix :: String 
pmTypePrefix = "PM"

patternMatches :: TypeSystem -> TypeSystem
patternMatches (Ts sig rules) = (Ts sig (map patternMatchesR rules))

patternMatchesR :: Rule -> Rule 
patternMatchesR (Rule premises conclusion) = (Rule newPremises conclusion) where newPremises = (fst (patternMatchesPrems 1 premises))

patternMatchesPrems :: Int -> [Premise] -> ([Premise], Int)
patternMatchesPrems n [] = ([], n)
patternMatchesPrems n ((Formula pred strings interms outterms):rest) = (((Formula pred strings interms outterms'):pmPremises ++ newPremises), z)
                    where
                    (outterms', pmPremises, m) = (patternMatchesTrms n outterms)
                    (newPremises, z) = (patternMatchesPrems m rest)                    
patternMatchesPrems n ((Hypothetical premise1 premise2):rest) = (((Hypothetical premise1 (head pmPremises)):(tail pmPremises) ++ newPremises), z) where
                    (pmPremises, m) = patternMatchesPrems n [premise2]
                    (newPremises, z) = (patternMatchesPrems m rest) 

patternMatchesTrms :: Int -> [Term] -> ([Term], [Premise], Int)
patternMatchesTrms n [] = ([],[], n)
patternMatchesTrms n (typ:rest) = (case typ of 
                         (Var var) -> let (typesRest, pmPremisesRest, z) = (patternMatchesTrms n rest) in ((Var var):typesRest, pmPremisesRest, z)
                         (Constructor c terms) -> 
                                      let (termsInner, premisesInner, m) = (patternMatchesTrms n terms) in 
                                        let freshVar = (pmTypePrefix ++ (show m)) in
                                         let (typesRest, pmPremisesRest, z) = (patternMatchesTrms (m+1) rest) in
                                              ((Var freshVar):typesRest, [(Formula "match" [c] [(Var freshVar)] termsInner)] ++ premisesInner ++ pmPremisesRest, z))

-- Given a pattern-maching variable, it searches its pattern-matching premise in the rule
patternMatchByVar :: Rule -> Term -> Maybe Premise
patternMatchByVar rule term = searchPremiseByPredAndVar rule "match" term

-- Given a pattern-matching premise, gives you the entire term. Care is given to put contravariant terms at their place.
reverseMatch :: Package -> Premise -> Term
reverseMatch pkg (Formula pred strings interms outterms) = if pred == "match" then (Constructor (head strings) outterms) else error "reverseMatch called with an argument that is not pattern-matching premise"

{-
where
             contravariants = take (contravariantArguments pkg (head strings)) outterms
             ordinary = drop (contravariantArguments pkg (head strings)) outterms
-}