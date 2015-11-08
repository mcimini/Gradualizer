module LazyUD where

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
                        [(Formula "value" [] [(Var "V")] []),
                        (Formula "ground" [] [(Var "G")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Var "G", Var "L", dyntype])] [])
                  )]

value_constrCast :: SignatureEntry -> [Rule]
value_constrCast (Decl c typ contraInfo contexts entries) = if entries == [] then [] else 
                 let newVarsT = map (\n -> Var ("T" ++ (show n))) [1 .. (length (entries))] in
                 let newVarsTticked = map (enc "") newVarsT in 
                 [(Rule
                        [(Formula "value" [] [(Var "V")] [])]
                        (Formula "value" [] [(Constructor "cast" [Var "V", Constructor c newVarsT, Var "L", Constructor c newVarsTticked])] [])
                  )]

auxiliaryDefinitions :: Signature -> [Rule]
auxiliaryDefinitions sig = (map ground_def (listOfTypesMinusDyn sig)) ++ (map getGroundOf_rules (listOfTypesMinusDyn sig)) ++ sameGround_rules

ground_def :: SignatureEntry -> Rule
ground_def (Decl c typ contraInfo contexts entries) = (Rule [] (Formula "ground" [] [(Constructor c (replicate (length entries) dyntype))] []))


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
                                [(Formula "=" [] [(Var "E"), (Constructor "cast" [Var "V", Var "G", Var "L1", dyntype])] []),
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G")] [])]
                                (Formula "step" [] [(Constructor "cast" [Var "E", dyntype, Var "L2", Var "G"])] [Var "V"])
                        ),
                        (Rule
                                [(Formula "=" [] [(Var "E"), (Constructor "cast" [Var "V", Var "G1", Var "L1", dyntype])] []),
                                (Formula "value" [] [(Var "V")] []),
                                (Formula "ground" [] [(Var "G1")] []),
                                (Formula "ground" [] [(Var "G2")] []),
                                (Negated (Formula "sameGround" [] [(Var "G1"), (Var "G2")] []))
                                ]
                                (Formula "step" [] [(Constructor "cast" [Var "E", dyntype, Var "L2", Var "G2"])] [blame (Var "L2")])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "A")] [(Var "G")]),
                                (Negated (Formula "ground" [] [(Var "A")] []))
                                ]
                                (Formula "step" [] [(Constructor "cast" [Var "V", Var "A", Var "L", dyntype])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", Var "A", Var "L", (Var "G")]), Var "G", Var "L", dyntype])])
                        ),
                        (Rule
                                [(Formula "value" [] [(Var "V")] []),
                                (Formula "getGroundOf" [] [(Var "A")] [(Var "G")]),
                                (Negated (Formula "ground" [] [(Var "A")] []))
                                ]
                                (Formula "step" [] [(Constructor "cast" [Var "V", dyntype, Var "L", Var "A"])]
                                                [(Constructor "cast" [(Constructor "cast" [Var "V", dyntype, Var "L", (Var "G")]), Var "G", Var "L",  Var "A"])])
                        )
                        ]

getGroundOf_rules :: SignatureEntry -> Rule
getGroundOf_rules (Decl c typ contraInfo contexts entries) =
                 let newVarsX = map (\n -> Var ("X" ++ (show n))) [1 .. (length (entries))] in 
                        (Rule []
                               (Formula "getGroundOf" [] [(Constructor c newVarsX)] [(Constructor c (replicate (length entries) dyntype))])
                        )


sameGround_rules :: [Rule]
sameGround_rules = [
                        (Rule
                                [(Formula "getGroundOf" [] [(Var "T1")] [(Var "X")]), (Formula "getGroundOf" [] [(Var "T2")] [(Var "X")])]
                                (Formula "sameGround" [] [(Var "T1"), (Var "T2")] [])
                        )]
