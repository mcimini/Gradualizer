module TypeSystemForCC where

import TypeSystem

sig_cast :: [SignatureEntry]
sig_cast = [Decl "cast" "term" [(Simple "type"), (Simple "type"), (Simple "label"), (Simple "term")]]

castR :: Rule 
castR = (Rule 		[(Formula "typeOf" [] [(Var "E")] [(Var "T1")])] 		(Constructor "cast" [] [(Var "T1"),(Var "T2"),(Var "Label"), (Var "E")]) 		(Var "T2"))



toTypeSystemForCC :: TypeSystem -> TypeSystem
toTypeSystemForCC ts = extendTypeSystem ts sig_cast [castR]
