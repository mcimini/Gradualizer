module CastInsertion where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import FinalType
import Static
import TypeSystemForCC

labelPrefix :: String
labelPrefix = "L"

typeOfCI :: String
typeOfCI = "typeOfCI"

sig_typeOfCI :: Signature
sig_typeOfCI = [Decl typeOfCI "o" Nothing Nothing [Simple "term", Simple "term", Simple "typ"]]

encodingSuffix :: String
encodingSuffix = "'"

castInsertion :: TypeSystem -> TypeSystem
castInsertion ts = castInsertion_ (toGradual ts) 

castInsertion_ :: TypeSystem -> TypeSystem
castInsertion_ (Ts sig rules) = (Ts sig (map (castInsertion_rule sig) rules))

castInsertion_rule :: Signature -> Rule -> Rule 
castInsertion_rule sig (Rule premises' conclusion) = (Rule (map castInsertion_prem premises) (castInsertion_concl pkg conclusion)) 
	where 
		premises = concat (map subtypeUnfold premises')
		pkg = (sig, (Rule premises conclusion))

castInsertion_prem :: Premise -> Premise
castInsertion_prem (Formula pred strings interms outterms) = if pred == typeOfGradual || pred == typeOfCC then (Formula typeOfCI strings interms ((enc pred (head interms)):outterms)) else (Formula pred strings interms outterms)
castInsertion_prem (Hypothetical premise1 premise2) = (Hypothetical (castInsertion_prem premise1) (castInsertion_prem premise2))

castInsertion_concl :: Package -> Premise -> Premise
castInsertion_concl pkg (Formula pred strings interms outterms) = if pred == typeOfGradual || pred == typeOfCC then (Formula typeOfCI strings interms ((encWithCasts pkg pred (head interms)):outterms)) else (Formula pred strings interms outterms)

-- At the moment, we have applications (R x) that will turn into (R' x). Therefore term2 in Application is left not encoded.
enc :: String -> Term -> Term
enc pred (Var varterm) = if pred == typeOfCC then (Var varterm) else (Var (varterm ++ encodingSuffix))
enc pred (Application term1 term2) = (Application (enc pred term1) term2)
enc pred (Constructor c terms) = (Constructor c (map (enc pred) terms))
enc pred other = other

-- To do: when you add parsing signatures, you have to query for term variables, only those are searched for casts.  
encWithCasts :: Package -> String -> Term -> Term
encWithCasts pkg pred (Var varterm) = case assignedTypeByVar (pkg_getRule pkg) (Var varterm) of
                           Just typ -> (castWrap pkg pred (Var varterm) typ (destinationType pkg typ))
                           Nothing -> (Var varterm) -- error ("ERROR: notfound" ++ varterm ++ (show (pkg_getRule pkg))) -- 
encWithCasts pkg pred (Constructor c terms) = (Constructor c (map (encWithCasts pkg pred) terms))

castWrap :: Package -> String -> Term -> Term -> Term -> Term 
castWrap pkg pred (Var variable) source target = if (isAbstraction pkg (Var variable)) then (Lambda "x" (cast (Application (Var variable) (appliedTermTo (pkg_getRule pkg) (Var variable)) ))) else cast (Var variable) where cast = \x -> makeCastTerm (enc pred x) source target

makeCastTerm :: Term -> Term -> Term -> Term  
makeCastTerm term source target = if source == target then term else (Constructor "cast" [term, source, (Var labelPrefix), target])

assignedTypeByVar :: Rule -> Term -> Maybe Term
assignedTypeByVar rule term = case searchPremiseByPredAndVar rule typeOf term of { Nothing -> Nothing ; Just (Formula pred strings interms outterms) -> Just (head outterms) }

appliedTermTo :: Rule -> Term -> Term
appliedTermTo rule term = case searchPremiseByPredAndVar rule typeOf term of { Nothing -> error "impossible case because assignedTypeByVar succeded" ; Just (Formula pred strings interms outterms) -> case head interms of (Application t1 t2) -> t2 }

subtypeUnfold :: Premise -> [Premise]
subtypeUnfold (Formula pred strings interms outterms) = if pred == "subtypeG" then [Formula "subtypeCI" [] interms [freshvar], Formula "flow" [] [interms !! 0, freshvar] []] else [(Formula pred strings interms outterms)] where freshvar = Var (head strings) 
subtypeUnfold other = [other]