module GenerateAuxiliaryPredicates where

import System.IO.Unsafe 
import Data.Unique
import Data.Char
import Data.List
import TypeSystem
import Static
import TypeSystemForCC

simples :: Int -> [TypeEntry]
simples n = map (\i -> Simple "typ") [1 .. n]

auxiliaryPred :: TypeSystem -> TypeSystem
auxiliaryPred ts = (Ts (sigOfNewThings ts) (rulesOfAuxiliaryPred ts))

sigOfNewThings :: TypeSystem -> Signature
sigOfNewThings (Ts sig rules) =  (map matchingDeclByDecl (listOfTypes sig)) ++ (nub (addFlow ++ addJoin)) ++ addStatic ++ addSubtyping 
                   where
                   addFlow = if (any flowNeededIn rules) then [Decl "flow" "o" Nothing Nothing [Simple "typ", Simple "typ"], Decl "join2" "o" Nothing Nothing [Simple "typ", Simple "typ",Simple "typ"]] else []
                   addJoin = map (\n -> Decl ("join" ++ (show n)) "o" Nothing Nothing (simples (n+1))) [2 .. (maximum (map neededJoins rules))]
                   addStatic = if (any staticNeededIn rules) then sig_static else []
                   addSubtyping = if (any subtypingNeededIn rules) then [Decl "subtypeG" "o" Nothing Nothing [Simple "typ", Simple "typ"], Decl "subtypeCI" "o" Nothing Nothing [Simple "typ", Simple "typ", Simple "typ"]] else []

matchingDeclByDecl :: SignatureEntry -> SignatureEntry
matchingDeclByDecl (Decl c typ contraInfo contexts arguments) = (Decl ("match" ++ (firstCharUP c)) "o" Nothing Nothing (simples ((length arguments)+1)))

rulesOfAuxiliaryPred :: TypeSystem -> [Rule]
rulesOfAuxiliaryPred (Ts sig rules) = (concat (map matchingRulesByDecl (listOfTypes sig))) ++ (joinAndFlow (Ts sig rules)) ++ (staticRules (Ts sig rules)) ++ subtypingRule rules

matchingRulesByDecl :: SignatureEntry -> [Rule]
matchingRulesByDecl (Decl c typ contraInfo contexts arguments) = let newVars = map (\n -> (Var ("T" ++ (show n)))) [1 .. (length arguments)] in [Rule [] (Formula "match" [c] [(Constructor c newVars)] newVars), Rule [] (Formula "match" [c] [dyntype] (replicate (length arguments) dyntype))]

{- This is for when you want to deal with types that accepts arguments that are not types. like records. 
(Formula "match" [c] [dyntype] (mapi dynamicsOnlyForTypes arguments))]
dynamicsOnlyForTypes :: Int -> TypeEntry -> Term
dynamicsOnlyForTypes i typeentry= case typeentry of (Simple typ) -> if typ == kindOftype then dyntype else (Var ("X" ++ (show i)))
-}

flowRule :: Rule 
flowRule = (Rule
            [Formula "join" ["2"] [(Var "X1"), Var "X2"] [Var "JoinX"]]
            (Formula "flow" [] [(Var "X1"), Var "X2"] []))

joinAndFlow :: TypeSystem -> [Rule]
joinAndFlow (Ts sig rules) = nub (addFlow ++ addJoin) -- nub is to not duplicate the rules for join2 that can be inserted both because of flow or joins.
            where
            addFlow = if (any flowNeededIn rules) then flowRule:(joinNary 2) else []
            addJoin = if (maximum (map neededJoins rules)) > 1 then (joinNary (maximum (map neededJoins rules))) ++ (map joinForConstructors (listOfTypes sig)) else []

joinNary :: Int -> [Rule]
joinNary 2 = [ (Rule [] (Formula "join" ["2"] [dyntype, Var "X"] [Var "X"])),
               (Rule [] (Formula "join" ["2"] [Var "X", dyntype] [Var "X"])),
               (Rule [] (Formula "join" ["2"] [Var "X", Var "X"] [Var "X"]))]

joinNary n = if (n >= 3) then (currentJoinRule:joinNary (n - 1)) else [] -- case n is 0. case n is 1 is impossible
         where
         newvars = map (\n -> Var ("X" ++ (show n))) [1 .. n]
         currentJoinRule = (Rule
                            [Formula "join" ["2"] [Var "X1", Var "X2"] [Var "Xtmp"],
                             (Formula "join" [show (n - 1)] ((Var "Xtmp"):(drop 2 newvars)) [Var "JoinX"])
                            ] 
                            (Formula "join" [(show n)] newvars [Var "JoinX"]))


joinForConstructors :: SignatureEntry -> Rule
joinForConstructors (Decl c typ contraEntries contexts arguments) = Rule premises conclusion
                    where 
                    newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (arguments))]
                    newVarsY = map (\n -> Var ("Y" ++ (show n))) [1 .. (length (arguments))]
                    newVarsZ = map (\n -> Var ("Z" ++ (show n))) [1 .. (length (arguments))]
                    conclusion = Formula "join" ["2"] [Constructor c newVarsX, Constructor c newVarsY] [Constructor c newVarsZ] 
                    premises = map singleJoinPremise (findIndices onlyTypes arguments)
                    singleJoinPremise = \index -> Formula "join" ["2"] [newVarsX !! index, newVarsY !! index] [newVarsZ !! index]
                    onlyTypes = \x -> case x of { (Simple typ) -> typ == kindOftype ; otherwise -> False }
	            

-- First check if premises is null, or maximum will throw exception 
neededJoins :: Rule -> Int
neededJoins (Rule premises conclusion) = if (null premises) then 0 else (maximum (map neededJoins_prem premises)) where neededJoins_prem = \prem -> case prem of {(Formula pred strings interms outterms) -> if pred == "join" then read (head strings)::Int else 0 ; otherwise -> 0}

staticRules :: TypeSystem -> [Rule]
staticRules (Ts sig rules) = if (any staticNeededIn rules) then (map staticRulesByConstructor (listOfTypesMinusDyn sig)) else []
            where staticRulesByConstructor = \decl -> case decl of (Decl c typ info contexts arguments) -> let newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (arguments))] in (Rule (map (\x -> staticWrapper x) newVarsX) (staticWrapper (Constructor c newVarsX)))

subtypingRule :: [Rule] -> [Rule] 
subtypingRule rules = if (any subtypingNeededIn rules) then
              [
			  (Rule
              	[(Formula "flow" [] [Var "X1", Var "X2"] []), (Formula "subtype" [] [Var "X2", Var "X3"] [])]
              	(Formula "subtypeG" [] [Var "X1", Var "X3"] [])), 
			  (Rule
              	[(Formula "flow" [] [Var "X1", Var "X2"] []), (Formula "subtype" [] [Var "X2", Var "X3"] [])]
              	(Formula "subtypeCI" [] [Var "X1", Var "X3"] [Var "X2"])) 			  
			  ]
              else []

flowNeededIn :: Rule -> Bool
flowNeededIn (Rule premises conclusion) = any (predicateIn "flow") premises
staticNeededIn :: Rule -> Bool
staticNeededIn (Rule premises conclusion) = any (predicateIn "static") premises
subtypingNeededIn :: Rule -> Bool
subtypingNeededIn (Rule premises conclusion) = any (predicateIn "subtypeG") premises

predicateIn :: String -> Premise -> Bool
predicateIn pred1 prem = case prem of {(Formula pred2 strings interms outterms) -> pred1 == pred2 ; otherwise -> False}
