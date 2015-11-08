module CastManagement where

import System.IO.Unsafe 
import Data.Unique
import Data.List
import Data.String.Utils
import TypeSystem
import Static
import TypeSystemForCC
import FinalType
import FlowDiscovery
import CastInsertion
import LazyUD as LUD
import LazyD as LD

sigOfdynamic :: Signature
sigOfdynamic = [Decl "ground" "o" Nothing Nothing [Simple "typ"],
                Decl "getGroundOf" "o" Nothing Nothing [Simple "typ", Simple "typ"],
                Decl "sameGround" "o" Nothing Nothing [Simple "typ", Simple "typ"],
                Decl "error" "o" Nothing Nothing [Simple "term"]
--                Decl "equal" "o" Nothing [Simple "term", Simple "term"]
               ]

castManagement :: CastStrategy -> TypeSystem -> TypeSystem
castManagement strategy (Ts sig rules) = renameToStepTickedIFNECESSARY (extend (castManagementGeneral strategy (Ts sig rules)) (Ts [] (concat (map (castForOperators strategy (Ts sig rules)) (onlyDeconstructorsOfHigher sig)))))

castManagementGeneral :: CastStrategy -> TypeSystem -> TypeSystem 
castManagementGeneral strategy (Ts sig rules) = case strategy of 
	LazyUDTwo -> (Ts sig ((LUD.valuesDefinitions sig) ++ LUD.blameIsError_rule ++ LUD.castManagement_rules ++ (LUD.auxiliaryDefinitions sig)))
	LazyDTwo -> (Ts sig ((LD.valuesDefinitions sig) ++ LD.blameIsError_rule ++ LD.castManagement_rules ++ LD.auxiliaryDefinitions))
	

-- Only deconstructors of higher order types ()
castForOperators :: CastStrategy -> TypeSystem -> SignatureEntry -> [Rule]
castForOperators strategy (Ts sig rules) (Decl c typ contraInfo contexts entries) = case (searchRuleByPredAndName (Ts sig rules) typeOfCC c) of (Rule premises conclusion) -> case conclusion of (Formula pred strings interms outterms) -> let canonical = (extractCanonicalType (Rule premises conclusion) premises) in let substitutions = substitutionsFromCanonical canonical (enc "" canonical) in let pkgSub = (makePkg sig (Rule substitutions conclusion)) in let flowPremises = (flowPremisesFromCasts pkgSub c canonical) in let pkgFlow = (makePkg sig (Rule flowPremises conclusion)) in let newTypeAnnotations = replaceTypeAnnotationWith pkgSub (head interms) in let newAssignedType = (destinationType pkgSub (head outterms)) in let newConclusion = (Formula pred strings [newTypeAnnotations] [newAssignedType]) in (extractOperationalRule (addWholeCast pkgFlow (castInsertion_rule sig (Rule ((map (castIrreplaceable canonical) premises) ++ flowPremises) newConclusion))) pkgFlow canonical (head interms)) 

-- in let (complement, leftovers) = (complementRules result flowPremises) in case result of (Rule resultPrems resultConcl) -> (Rule (resultPrems ++ leftovers) resultConcl):complement


addWholeCast :: Package -> Rule -> Rule
addWholeCast pkg (Rule premises conclusion) = let originalTerm = getFirstInputOfPremise conclusion in let [castTerm,provedType] = getOutputsOfPremise conclusion in let newAssignedType = (destinationType pkg provedType) in (Rule [] (Formula typeOfCI [] [originalTerm] [makeCastTerm (replaceDeconstructingTerm pkg castTerm (Var "V")) provedType newAssignedType, newAssignedType]))

--error ((show pkg) ++ (show provedType) ++ (show newAssignedType) ++ (show (Rule [] (Formula typeOfCI [] [originalTerm] [makeCastTerm (replaceDeconstructingTerm pkg castTerm (Var "V")) provedType newAssignedType, newAssignedType]))))
-- error ((show (castTerm)) ++ (show ((replaceDeconstructingTerm pkg castTerm (Var "V")))))

castIrreplaceable :: Term -> Premise -> Premise
castIrreplaceable canonical premise = case premise of { (Hypothetical (Formula pred1 strings1 interms1 [typ]) (Formula pred2 strings2 [Application var applied] outterms2)) -> if elem typ (varsOf canonical) then (Hypothetical (Formula pred1 strings1 interms1 [typ]) (Formula pred2 strings2 [Application var (makeCastTerm applied typ (enc "" typ))] outterms2)) else premise ; otherwise -> premise }

extractOperationalRule :: Rule -> Package -> Term -> Term -> [Rule]
extractOperationalRule (Rule premises conclusion) pkg canonical original = let castOnV = makeCastTerm (Var "V") (enc "" canonical) canonical in case conclusion of (Formula pred strings intermsRule outtermRule) -> let target = (head outtermRule) in [(Rule [(Formula "value" [] [(Var "V")] [])] (Formula "step" [] [replaceDeconstructingTerm pkg original castOnV] [target]))]

substitutionsFromCanonical :: Term -> Term -> [Premise]
substitutionsFromCanonical (Constructor c1 terms1) (Constructor c2 terms2) = (zipWith makeFlow terms1 terms2)

extractCanonicalType :: Rule -> [Premise] -> Term
extractCanonicalType cc [] = error ("ERROR extractCanonicalType: I did not find a deconstructed type in the rule" ++ (show cc))
extractCanonicalType cc (premise:rest) = case premise of (Formula pred info interms outterms) -> if pred == typeOfCC then case (head outterms) of { (Constructor c terms) -> (Constructor c terms) ; otherwise -> extractCanonicalType cc rest } else extractCanonicalType cc rest

flowPremisesFromCasts :: Package -> String -> Term -> [Premise]
flowPremisesFromCasts pkg c_app canonical = case canonical of (Constructor c_arrow terms) -> let encArguments = map (enc "") terms in let split = contravariantArguments pkg c_arrow in (zipWith makeFlow (take split terms) (take split encArguments)) ++ (zipWith makeFlow (drop split encArguments) (drop split terms))

replaceTypeAnnotationWith :: Package -> Term -> Term
replaceTypeAnnotationWith pkg (Constructor c terms) = let numero = numberOfTypeAnnotations (pkg_getSig pkg) c in let newTerms = (map (destinationType pkg) (take numero terms)) ++ (drop numero terms) in (Constructor c newTerms) 

complementRules :: Rule -> [Premise] -> ([Rule], [Premise])
complementRules (Rule premises conclusion) flowPremises = let leftovers = (flowPremises \\ (flowsInside (getFirstOutputOfPremise conclusion))) in ((map createComplementRule leftovers), leftovers) where createComplementRule = \premise -> (Rule [Negated premise] (Formula "step" [] [(getFirstInputOfPremise conclusion)] [blame (Var "L")]))

flowsInside :: Term -> [Premise]
flowsInside (Var variable) = []
flowsInside (Constructor c terms) = (if c == "cast" then [(Formula "flow" [] [(terms !! 1), (terms !! 3) ] []), (Formula "flow" [] [(terms !! 3), (terms !! 1) ] [])] ++ rest else rest) where rest = (concat (map flowsInside terms))
flowsInside (Application term1 term2) = flowsInside term1 ++ flowsInside term2
flowsInside (Bound x) = []
flowsInside (Lambda bound term) = flowsInside term

renameToStepTickedIFNECESSARY :: TypeSystem -> TypeSystem
renameToStepTickedIFNECESSARY (Ts sig rules) = case searchDeclByName sig "step" of {Nothing -> error "ERROR: There is no predicate step" ; Just entry -> case entry of  (Decl c2 typ contraInfo contexts entries) -> case entries of [(Simple "term"),(Simple "term")] -> (Ts sig rules) ; otherwise -> (Ts sig ((map (renamingInRule "step" "step'") (rules)) ++ (wrappedStep' otherwise)))}

wrappedStep' :: [TypeEntry] -> [Rule]
wrappedStep' typeentries = case findIndices (\x -> (Simple "term") == x) typeentries of [n1, n2] -> [(Rule [Formula "step'" [] [Var "E"] [Var "E'"]] (Formula "step" [] ((Var "E"):createVars) ((Var "E'"):createVars)))] where createVars = map (\n -> Var ("X" ++ (show n))) [1 .. (n2-1)]
--(Formula "step" [] (mapi (createvar n1 n2) typeentries) []))] where createvar = \n1 -> \n2 -> \i -> \entry -> if i == n1 then Var "E" else if i == n2 then Var "E'" else Var ("X" ++ (show (i `mod` n2)))

