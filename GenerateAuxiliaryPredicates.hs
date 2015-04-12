module GenerateAuxiliaryPredicates where

import System.IO.Unsafe 
import Data.Unique
import Data.List
import TypeSystem
import ToLambdaProlog


gensymX :: IO String
gensymX = do 
			sym <- newUnique
			return ("X" ++ show (hashUnique sym))

dyntypeEncoding :: Term
dyntypeEncoding = (Constructor "dyn" [] [])

matchesRulesIO :: String -> TypeSystem -> IO ()
matchesRulesIO mode ts = putStrLn (matchesRules mode' ts)
							where mode' = case mode of
								"Original" -> "O" 
								"O" -> "O" 
								"Gradual" -> "Gr" 
								"Gr" -> "Gr" 
								

matchesRules :: String -> TypeSystem -> String
matchesRules mode ts = unlines (map (matchesRules_ mode) (listOfTypes ts))

listOfTypes :: TypeSystem -> [SignatureEntry]
listOfTypes (Ts sig rules) = let onlytypes = (\x -> case x of 
												(Decl c typ arguments) -> if typ == "typ" then True else False) in 
									(filter onlytypes sig)

matchesRules_ :: String -> SignatureEntry -> String
matchesRules_ mode sigone = let plainPM = (matchesRules_Original mode sigone) in 
								if mode == "Gr" then unlines [plainPM, (matchesRules_Gradual mode sigone)]
								else plainPM

matchesRules_Original :: String -> SignatureEntry -> String
matchesRules_Original mode (Decl c typ arguments) = let newVars = map (\n -> (Var ("X" ++ (show n)))) [1 .. (length arguments)] in
												if typ == "typ" then (toLambdaPrologPr mode (Formula "match" [c] [(Constructor c [] newVars)] newVars)) ++ "."
												else error "ERROR: Trying to build a pattern-match rule not for a type. Probable: Filter for types of signature does not work, returned a term"

matchesRules_Gradual :: String -> SignatureEntry -> String
matchesRules_Gradual mode (Decl c typ arguments) = let dynamicsOnlyForTypes = (\x -> case x of (Simple tipo) -> if tipo == typ then dyntypeEncoding else (freshVar x)) in
												if typ == "typ" then (toLambdaPrologPr mode (Formula "match" [c] [dyntypeEncoding] (map dynamicsOnlyForTypes arguments))) ++ "."
												else error "ERROR: Trying to build a pattern-match rule not for a type. Probable: Filter for types of signature does not work, returned a term"
												where
												freshVar = \x -> (Var (unsafePerformIO gensymX))

consistencyRule :: String 
consistencyRule = "consistency X1 X2 :- join2 X1 X2 JoinX."											

joinRulesIO :: TypeSystem -> IO ()
joinRulesIO ts = do 
					putStrLn (joinRules ts)
					putStrLn consistencyRule

joinRules :: TypeSystem -> String 
joinRules ts = if (max > 1) then (unlines (joinNary max)) ++ (unlines (map joinForConstructors (listOfTypes ts))) else ""
				where 
				max = if needed < 2 then 2 else needed
				needed = (neededJoins ts) 

joinNary :: Int -> [String]
joinNary 2 = ["join2 X " ++ lprologDynamic ++ " X.", "join2 " ++ lprologDynamic ++ " X X.", "join2 X X X."]				
joinNary n = if (n >= 3) then (currentJoinRule:joinNary (n - 1)) else error "Error, trying to generate join2 with n-ary case" 
			where
				newvars = map (\n -> " X" ++ (show n)) [1 .. n]
				currentJoinRule = "join" ++ (show n) ++ (concat newvars) ++ " Xjoin :- join2 X1 X2 Xtmp, " ++ continuation ++ "."
				continuation = "join" ++ (show (n - 1)) ++ " Xtmp" ++ (concat (drop 2 newvars)) ++ " Xjoin"

joinForConstructors :: SignatureEntry -> String
joinForConstructors (Decl c typ arguments) =  conclusion ++ premises ++ "."
									where 
									conclusion = "join2 (" ++ c ++ (concat newVarsX) ++ ") (" ++ c ++ (concat newVarsY) ++ ") (" ++ c ++ (concat newVarsZ) ++ ")"
									newVarsX = map (\n -> " X" ++ (show n)) [1 .. (length (arguments))]
									newVarsY = map (\n -> " Y" ++ (show n)) [1 .. (length (arguments))]
									newVarsZ = map (\n -> " Z" ++ (show n)) [1 .. (length (arguments))]
									premises = if (length (premisesListOnlyTypes) > 0) then " :- " ++ intercalate ", " premisesListOnlyTypes else ""
									premisesList = zipWith3 wrap newVarsX newVarsY newVarsZ 
									wrap = \x -> \y -> \z -> ("join2" ++ x ++ y ++ z)
									premisesListOnlyTypes = filter toRemove (zipWith onlyTypes arguments premisesList)
									onlyTypes = \x -> \y -> case x of (Simple tipo) -> if tipo == "typ" then y else "toRemove"
									toRemove = \x -> not (x == "toRemove")
									

neededJoins :: TypeSystem -> Int
neededJoins ts = if (null onlytypes) then 0 else maximum (map arity (listOfTypes ts))
							where 
							arity = \x -> (case x of (Decl c typ arguments) -> (length arguments))
							onlytypes = (listOfTypes ts)

