module LazyD where

import Data.String.Utils
import Data.Char
import Data.List
import TypeSystem
import Static
import TypeSystemForCC
import CastInsertion


valuesDefinitions :: Signature -> [Rule]
valuesDefinitions sig = value_groundCast ++ (concat (map value_constrCast (listOfTypesMinusDyn sig)))

value_groundCast :: [Rule]
value_groundCast = [(Rule
                        [(Formula "value" [] [(Var "V")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Var "T", Var "L", dyntype])] []) 
					)]

value_constrCast :: SignatureEntry -> [Rule]
value_constrCast (Decl c typ contraInfo contexts entries) = if entries == [] then [] else 
                 let newVarsT = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in
                 let newVarsTticked = map (enc "") newVarsT in 
                 [(Rule
                        [(Formula "value" [] [(Var "V")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Constructor c newVarsT, Var "L", Constructor c newVarsTticked])] [])
                  )]

auxiliaryDefinitions :: [Rule]
auxiliaryDefinitions = []

blameIsError_rule :: [Rule]
blameIsError_rule = [(Rule []
                        (Formula "error" [] [(Constructor "blame" [Var "L"])] [])
                  )]

castManagement_rules :: [Rule]
castManagement_rules = [
                        (Rule
                                [(Formula "value" [] [(Var "V")] [])]
                                (Formula "step" [] [(Constructor "cast" [Var "V", Var "T", Var "L", Var "T"])] [Var "V"])
                        ),
                        (Rule
                                [(Formula "=" [] [(Var "E"), (Constructor "cast" [Var "V", Var "A", Var "L1", Var "B"])] []),
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "flow" [] [(Var "A"), (Var "C")] [])]
                                (Formula "step" [] [(Constructor "cast" [Var "E", Var "B", Var "L2", Var "C"])] [(Constructor "cast" [Var "V", Var "A", Var "L2", Var "C"])])
                        ),
                        (Rule
                                [(Formula "=" [] [(Var "E"), (Constructor "cast" [Var "V", Var "A", Var "L1", Var "B"])] []),
                                (Formula "value" [] [(Var "V")] []),
                                (Negated (Formula "flow" [] [(Var "A"), (Var "C")] []))]
                                (Formula "step" [] [(Constructor "cast" [Var "E", Var "B", Var "L2", Var "C"])] [blame (Var "L2")])
                        )
                        ]
