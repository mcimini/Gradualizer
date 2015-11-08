module TypeSystem where

import Data.String.Utils
import Data.Char
import Data.List

type Signature = [SignatureEntry]
type LogicalLine = (String, (Int, Int))
type ContextInfo = Maybe [(Int,[Int])]

data CastStrategy = LazyUDTwo
          | LazyDTwo 
               deriving (Show, Eq, Ord)

data Info = Constr String
          | Deconstructor [LogicalLine]
          | Derived 
          | DerivedDec [LogicalLine]
               deriving (Show, Eq, Ord)

data TypeEntry = Simple String
               | Abs String String
               deriving (Show, Eq, Ord)

data SignatureEntry = Decl String String (Maybe Info) ContextInfo [TypeEntry]
     deriving (Show, Eq, Ord)
				

data Term = Var String
          | Constructor String [Term]
          | Application Term Term
          | Lambda String Term
          | Bound String
          deriving (Show, Eq, Ord)


data Premise = Formula String [String] [Term] [Term]
             | Hypothetical Premise Premise
             | Negated Premise 
             deriving (Show, Eq, Ord)
				

data Rule = Rule [Premise] Premise
     deriving (Show, Eq, Ord)
			

data TypeSystem = Ts Signature [Rule] 
     deriving (Show, Eq, Ord)


signatureOf :: TypeSystem -> Signature
signatureOf (Ts sig rules) = sig

extend :: TypeSystem -> TypeSystem -> TypeSystem
extend (Ts sig rules) (Ts newsig newrules) = (Ts (nub (sig ++ newsig)) (nub (rules ++ newrules)))

extendTypeSystem :: TypeSystem -> Signature -> [Rule] -> TypeSystem
extendTypeSystem (Ts sig rules) newsig newrules = (Ts (sig ++ newsig) (rules ++ newrules))

toStringT :: Term -> String
toStringT (Var variable) = variable
toStringT term = error "Error: toStringT is used for a complex term. Probably outputs does not contain variables."

toStringTE :: TypeEntry -> String
toStringTE (Simple item) = item
toStringTE term = error "Error: toStringTE is used for an abstraction. Probably the signature is ill-formed."

kindOftype :: String
kindOfterm :: String
typeOf :: String
typeOfGradual :: String
kindOftype = "typ"
kindOfterm = "term"
typeOf = "typeOf"
typeOfGradual = typeOf ++ "Gr"

renamingInRule :: String -> String -> Rule -> Rule
renamingInRule pred1 pred2 (Rule premises conclusion) = (Rule (map (renamingInRule_prem pred1 pred2) premises) (renamingInRule_prem pred1 pred2 conclusion))
renamingInRule_prem :: String -> String -> Premise -> Premise
renamingInRule_prem pred1 pred2 premise = case premise of { (Formula pred strings interms outterms) -> if pred == pred1 then (Formula pred2 strings interms outterms) else premise ; (Hypothetical premise1 premise2) -> (Hypothetical (renamingInRule_prem pred1 pred2 premise1) (renamingInRule_prem pred1 pred2 premise2)) ; otherwise -> premise}

varsOf :: Term -> [Term]
varsOf (Var variable) = [(Var variable)]
varsOf (Constructor c terms) = (concat (map varsOf terms)) 
varsOf (Application term1 term2) = varsOf term1 ++ varsOf term2
varsOf (Bound x) = []

provedOperator :: Rule -> String
provedOperator (Rule premises conclusion) = case conclusion of (Formula pred1 strings interms outterms) -> case (head interms) of { (Constructor c terms) -> c ; otherwise -> error (show (Rule premises conclusion)) }

listOfTypes :: Signature -> Signature
listOfTypes sig = let onlytypes = (\x -> case x of (Decl c typ contraInfo contexts entries) -> typ == "typ") in (filter onlytypes sig)

listOfTerms :: Signature -> Signature
listOfTerms sig = let onlyterms = (\x -> case x of (Decl c typ contraInfo contexts entries) -> typ == "term") in (filter onlyterms sig)

onlyDeconstructors :: Signature -> Signature
onlyDeconstructors sig = let onlyDec = (\x -> case x of (Decl c typ contraInfo contexts entries) -> case contraInfo of { Just (Deconstructor triples) -> True ; otherwise -> False }) in (filter onlyDec sig)

onlyConstructors :: Signature -> Signature
onlyConstructors sig = let onlyCons = (\x -> case x of (Decl c typ contraInfo contexts entries) -> case contraInfo of { Just (Constr something) -> True ; otherwise -> False }) in (filter onlyCons sig)

onlyDeconstructorsOfHigher :: Signature -> Signature
onlyDeconstructorsOfHigher sig = let onlyHigherTypes = (\x -> case x of (Decl c typ (Just (Deconstructor ((c2, (n1,n2)):rest))) contexts entries) -> case searchDeclByName sig c2 of { Nothing -> False ; Just (Decl c2 typ contraInfo contexts entries) -> (length entries) > 0 } ) in filter onlyHigherTypes (onlyDeconstructors sig)

searchDeclByName :: Signature -> String -> Maybe SignatureEntry
searchDeclByName [] c = Nothing
searchDeclByName (entry:rest) c = case entry of Decl c1 typ contraInfo contexts entries -> if c1 == c then Just entry else searchDeclByName rest c 

deleteDeclByName :: Signature -> String -> Signature
deleteDeclByName [] c = []
deleteDeclByName (entry:rest) c = case entry of Decl c1 typ contraInfo contexts entries -> if c1 == c then rest else entry:(deleteDeclByName rest c)

type Package = (Signature, Rule)

makePkg :: Signature -> Rule -> Package
makePkg sig rule = (sig, rule)

pkg_getSig :: Package -> Signature
pkg_getSig pkg = fst pkg

pkg_getRule :: Package -> Rule
pkg_getRule pkg = snd pkg

isContravariant :: Package -> String -> Bool
isContravariant pkg c = (contravariantArguments pkg c) > 0

contravariantArguments :: Package -> String -> Int
contravariantArguments pkg c = case searchDeclByName (pkg_getSig pkg) (provedOperator (pkg_getRule pkg)) of { Nothing -> 0 ; Just entry -> case entry of Decl c1 typ contraInfo contexts entries -> case contraInfo of { Nothing -> 0 ; Just (Constr str) -> 0 ; Just (Deconstructor triples) -> case lookup c triples of { Nothing -> 0 ; Just (n1,n2) -> n2} ; Just (Derived) -> 0 ; Just (DerivedDec triples) -> case lookup c triples of { Nothing -> 0 ; Just (n1,n2) -> n2} } }

userTypeArguments :: Package -> String -> Int
userTypeArguments pkg c = case searchDeclByName (pkg_getSig pkg) c of { Nothing -> error "ERROR: userTypeArguments, not found" ; Just entry -> case entry of Decl c1 typ contraInfo contexts entries -> count (\entry -> case entry of { Simple tt -> tt == kindOftype ; otherwise -> False }) entries } where count = \p -> length . filter p

isTypeConstructor :: Package -> String -> Bool
isTypeConstructor pkg c = case searchDeclByName (pkg_getSig pkg) c of { Nothing -> error ("isTypeConstructor: not found " ++ c ++ (show (pkg_getSig pkg))) ; Just entry -> case entry of Decl c1 typ contraInfo contexts entries -> typ == kindOftype }


-- I need the addition: (n + length typeannotations) because in letrec T1 R1, R1 is the first of programs and have n = 0. The lookup of (Abs term1 term2) is compromised then.
isAbstraction :: Package -> Term -> Bool
isAbstraction pkg (Var variable) = case (pkg_getRule pkg) of (Rule premises conclusion) -> case conclusion of (Formula pred1 strings interms outterms) -> case (head interms) of (Constructor c terms) -> case (elemIndex (Var variable) terms) of { Nothing -> error (concat (map toStringT terms)) ; Just n -> case (searchDeclByName (pkg_getSig pkg) c) of { Nothing -> error "ERROR: isAbstraction failed to find the constructor in thesignature" ; Just (Decl c1 typ contraInfo contexts entries) -> case (entries !! n) of { Simple term -> False ; (Abs term1 term2) -> True } } }

searchRuleByPredAndName :: TypeSystem -> String -> String -> Rule 
searchRuleByPredAndName (Ts sig rules) pred1 c1 = head (filter myrule rules) where myrule = \rule -> case rule of (Rule premises conclusion) -> case conclusion of (Formula pred2 info interms outterms) -> equalOrTypeOf pred1 pred2 && (let c2 = (case (head interms) of (Constructor c3 terms) -> c3) in c1 == c2 )

-- Given a predicate and a term, it returns the first premise that uses the predicate and the term as in input.
-- Example: typeOf E (T1 -> T2) is returned when searched with searchPremiseByPredAndVar rule "typeOf" (Var "E")
searchPremiseByPredAndVar :: Rule -> String -> Term -> Maybe Premise
searchPremiseByPredAndVar (Rule premises conclusion) pred variable = searchPremiseByPredAndVar_prem premises pred variable
searchPremiseByPredAndVar_prem :: [Premise] -> String -> Term -> Maybe Premise
searchPremiseByPredAndVar_prem [] pred variable = Nothing
searchPremiseByPredAndVar_prem ((Formula pred2 strings interms outterms):rest) pred1 variable = if equalOrTypeOf pred1 pred2 && elem variable (varsOf (head interms)) then Just (Formula pred2 strings interms outterms) else searchPremiseByPredAndVar_prem rest pred1 variable -- error (pred1 ++ pred2) 
searchPremiseByPredAndVar_prem ((Hypothetical premise1 premise2):rest) pred variable = searchPremiseByPredAndVar_prem (premise2:rest) pred variable


getFirstInputOfPremise :: Premise -> Term
getFirstInputOfPremise (Formula pred strings interms outterms) = head interms

getFirstOutputOfPremise :: Premise -> Term
getFirstOutputOfPremise (Formula pred strings interms outterms) = head outterms

getOutputsOfPremise :: Premise -> [Term]
getOutputsOfPremise (Formula pred strings interms outterms) = outterms

equalOrTypeOf :: String -> String -> Bool
equalOrTypeOf pred1 pred2 = (pred1 == pred2) || (startswith typeOf pred1 && startswith typeOf pred2)

firstCharUP [] = []
firstCharUP (h:t) = (toUpper h):t

removePredicateFromTS :: TypeSystem -> String -> TypeSystem
removePredicateFromTS (Ts sig rules) pred = (Ts (deleteDeclByName sig pred) (filter (not . (filterRules pred)) (map (removePredicateFromRule pred) rules)))

removePredicateFromRule :: String -> Rule -> Rule
removePredicateFromRule pred1 (Rule premises conclusion) = (Rule (filter noPred premises) conclusion) where noPred = \premise -> case premise of { (Formula pred2 strings interms outterms) -> not (pred1 == pred2) ; _ -> True }

filterRules :: String -> Rule -> Bool
filterRules pred1 (Rule premises conclusion) = case conclusion of { (Formula pred2 strings interms outterms) -> (pred1 == pred2) || (tickedpred1 == pred2) ; _ -> False } where tickedpred1 = pred1 ++ "'"

numberOfTypeAnnotations :: Signature -> String -> Int
numberOfTypeAnnotations [] c = error "ERROR: numberOfTypeAnnotations. Did not find the constructor in the signature"
numberOfTypeAnnotations (entry:rest) c1 = case entry of (Decl c2 typ contraInfo contexts entries) -> if c1 == c2 then length $ filter (istype) entries else numberOfTypeAnnotations rest c1 where istype = \entry -> entry == (Simple "typ")

replaceDeconstructingTerm :: Package -> Term -> Term -> Term
replaceDeconstructingTerm pkg original term = case original of (Constructor c_app outterms) -> let numero = numberOfTypeAnnotations (pkg_getSig pkg) c_app in let newOutTerms = (take numero outterms) ++ [term] ++ (drop (numero + 1) outterms) in (Constructor c_app newOutTerms)

rulesForConstructors :: Signature -> [Rule] -> [Rule]
rulesForConstructors sig rules = let ts = (Ts (onlyConstructors sig) rules) in map (searchRuleByPredAndName ts typeOf) (map getC (onlyConstructors sig)) where getC = \entry -> case entry of (Decl c typ contraInfo contexts entries) -> c

doesItAppear :: String -> TypeSystem -> Bool
doesItAppear c (Ts sig rules) = (any (doesItAppear_rule c) rules)-- (filter (filterRules "step") rules)) && (case searchDeclByName sig c of { Nothing -> True ; Just something -> False} )
doesItAppear_rule c (Rule premises conclusion) = case conclusion of (Formula pred strings interms outterms) -> pred == c || any (doesItAppear_trm c) outterms
doesItAppear_trm c1 (Constructor c2 terms) = if c1 == c2 then True else any (doesItAppear_trm c1) terms
doesItAppear_trm c1 (Application term1 term2) = any (doesItAppear_trm c1) [term1,term2]
doesItAppear_trm c1 (Lambda bound term) = any (doesItAppear_trm c1) [term]
doesItAppear_trm c1 otherwise = False

extractContexts :: Signature -> [(String, (Int, [Int]), Int)]
extractContexts [] = []
extractContexts ((Decl c typ contraInfo contexts entries):rest) = case contexts of {Nothing -> extractContexts rest ; Just positions -> (map (\p -> (c, p, (length entries))) positions) ++  extractContexts rest}

anyContextInfo :: SignatureEntry -> Bool
anyContextInfo (Decl c typ contraInfo contexts entries) = case contexts of {Nothing -> False ; Just positions -> True }

mapi f l = zipWith f [0..] l 