module TypeSystemForCC where

import TypeSystem
import Static

dyntype :: Term
dyntype = (Constructor "dyn" [])

blame :: Term -> Term
blame label = (Constructor "blame" [label])

typeOfCC :: String
typeOfCC = typeOf ++ "CC"

sig_castCalculus :: [SignatureEntry]
sig_castCalculus = [Decl typeOfCC "o" Nothing Nothing [Simple "term", Simple "typ"],
                    Decl "cast" "term" Nothing (Just [(1, [])]) [(Simple "term"), (Simple "typ"), (Simple "label"), (Simple "typ")],
                    Decl "blame" "term" Nothing Nothing [(Simple "label")]
                    ]

castR :: [Rule] 
castR = [(Rule
                [(Formula "typeOf" [] [(Var "E")] [(Var "T1")])]
                (Formula "typeOf" [] [(Constructor "cast" [(Var "E"),(Var "T1"),(Var "L"), (Var "T2")])] [(Var "T2")])),
         (Rule
                []
                (Formula "typeOf" [] [(Constructor "blame" [(Var "L")])] [(Var "T")])
          )]


toTypeSystemForCC :: TypeSystem -> TypeSystem
toTypeSystemForCC (Ts sig rules) = (Ts (sig ++ sig_castCalculus) (map (renamingInRule typeOf typeOfCC) (rules ++ castR)))
