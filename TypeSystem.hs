module TypeSystem where
import Data.String.Utils

data TypeEntry = Simple String
				| Abs String String
				deriving (Show, Eq, Ord)


data SignatureEntry = Decl String String [TypeEntry]
				deriving (Show, Eq, Ord)
				

data Term = Var String
			| Constructor String [Term] [Term]
			| Application Term Term
			| Lambda String Term
			| Bound String
			| Encode Term Term 
			deriving (Show, Eq, Ord)


data Premise = Formula String [String] [Term] [Term]
			| Hypothetical String Term Term Term Term
			deriving (Show, Eq, Ord)
				

data Rule = Rule [Premise] Term Term
			deriving (Show, Eq, Ord)
			

data TypeSystem = Ts [SignatureEntry] [Rule] 
				  deriving (Show, Eq, Ord)

{- Returns the position of the type input. 0 is none. 
This supports only one type input per constructor. -}

sig :: String -> String -> Bool
sig c kind = if (kind == "type") && (c == "arrow") then True else False

typePrefix :: String
typePrefix = "T"

outputTypePrefix :: String
outputTypePrefix = "O"

pmTypePrefix :: String
pmTypePrefix = "PM"

termPrefix :: String
termPrefix = "E"

abstractionPrefix :: String
abstractionPrefix = "R"

labelPrefix :: String
labelPrefix = "L"


isType :: Term -> Bool
isType (Var variable) = startswith typePrefix variable
isType (Constructor c inputsCapab outputsCapab) = (sig c "type")

isOutputType :: Term -> Bool
isOutputType (Var variable) = ((head variable) == (head outputTypePrefix))
isOutputType term = False

isAbstraction :: Term -> Bool
isAbstraction (Var variable) = ((head variable) == (head abstractionPrefix))
isAbstraction term = False

isTerm :: Term -> Bool
isTerm (Var variable) = ((head variable) == (head termPrefix))
isTerm term = False

isTermExtended :: Term -> Bool
isTermExtended (Var variable) = isTerm (Var variable) || isAbstraction (Var variable)
isTermExtended term = False


extendTypeSystem :: TypeSystem -> [SignatureEntry] -> [Rule] -> TypeSystem
extendTypeSystem (Ts sig rules) newsig newrules = (Ts (sig ++ newsig) (rules ++ newrules))

signatureOf :: TypeSystem -> [SignatureEntry]
signatureOf (Ts sig rules) = sig

toStringT :: Term -> String
toStringT (Var variable) = variable
toStringT term = error "Error: toStringT is used for a complex term. Probably outputs does not contain variables."

isUserPredicate :: String -> Bool
isUserPredicate pred = case pred of 
						"consistency" -> False
						"match" -> False
						"join" -> False
						otherwise -> True

isVarPresent :: Term -> Term -> Bool
isVarPresent (Var variable1) (Var variable2) = (variable1 == variable2)
isVarPresent (Var variable) (Constructor c interms outterms) = (any (isVarPresent (Var variable)) (interms ++ outterms))
isVarPresent (Var variable) (Application term1 term2) = (any (isVarPresent (Var variable)) [term1,term2])
isVarPresent (Var variable) (Bound x) = False

allVariables :: Premise -> [String]
allVariables (Formula pred strings interms outterms) = (concat (map allVariablesTrm interms)) ++ (concat (map allVariablesTrm outterms))
allVariables (Hypothetical x term1 type1 term2 type2) = (allVariablesTrm term1) ++ (allVariablesTrm type1) ++ (allVariablesTrm term2) ++ (allVariablesTrm type2)

allVariablesTrm :: Term -> [String]
allVariablesTrm (Var variable) = [variable]
allVariablesTrm (Constructor c interms outterms) = (concat (map allVariablesTrm interms)) ++ (concat (map allVariablesTrm outterms)) 
allVariablesTrm (Application term1 term2) = (allVariablesTrm term1) ++ (allVariablesTrm term2)
allVariablesTrm (Bound x) = []
allVariablesTrm (Encode term1 term2) = (allVariablesTrm term1) ++ (allVariablesTrm term2)




