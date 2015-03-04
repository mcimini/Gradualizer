module CastInsertion where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching
import UpToConsistency
import ResolutionAndStatic

type CastSummary = (HM.Map Term Term)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

thd3 :: (a, b, c) -> c
thd3 (_,_,x) = x


gensymLabel :: IO String
gensymLabel = do 
			sym <- newUnique
			return (labelPrefix ++ show (hashUnique sym))

encodingSuffix :: String
encodingSuffix = "'"

castInsertion :: TypeSystem -> TypeSystem
castInsertion ts = (Ts sig rules')
									where
									(Ts sig rules1) = toGradual ts
									castSummaryByRules = (map getCastSummaryR rules1)
									rules' = zipWith castInsertionR castSummaryByRules rules1

getCastSummaryR :: Rule -> CastSummary
getCastSummaryR (Rule [] term typ) = HM.empty
getCastSummaryR (Rule (premise:rest) term typ) = HM.union (getCastSummaryPr premise) (getCastSummaryR (Rule rest term typ))

getCastSummaryPr :: Premise -> CastSummary
getCastSummaryPr (Formula pred strings interms outterms) = case pred of 
										"typeOf" -> (HM.insert (head encodedIn) (head encodedOut) $ HM.empty)
										"match" -> (HM.insert (head encodedIn) (Constructor (head strings) [] ((tail interms) ++ outterms)) $ HM.empty)
										"join" -> let flowpremises = (joinToFlow interms (head outterms)) in (foldl1 HM.union (map getCastSummaryPr flowpremises))
										"consistency" -> (HM.insert (head outterms) (head (tail outterms)) $ HM.empty)
										otherwise -> HM.empty
										where 
										encodedIn = (map enc interms)
										encodedOut = (map enc outterms)
getCastSummaryPr (Hypothetical bound outterm1 interm1 interm2 outterm2) = (HM.insert (enc (extractAbstraction interm2)) outterm2 $ HM.empty)
										where extractAbstraction = \x -> case x of 
																		(Var term) -> (Var term)
																		(Application (Var term) term2) -> (Var term)

castInsertionR :: CastSummary -> Rule -> Rule 
castInsertionR castSummary (Rule premises term typ) = (Rule premises' (Encode term (withCasts castSummary (enc term))) typ)
											where 
											premises' = map (castInsertionPr castSummary) premises

castInsertionPr :: CastSummary -> Premise -> Premise
castInsertionPr castSummary premise = case premise of 
							(Formula "typeOf" strings interms outterms) -> (Formula "typeOf" [] [(Encode (head interms) (enc (head interms)))] outterms)
							(Hypothetical bound outterm1 interm1 interm2 outterm2) -> (Hypothetical bound outterm1 interm1 (Encode interm2 (enc interm2)) outterm2)
							otherwise -> premise

enc :: Term -> Term
enc (Var varterm) = if (isTermExtended (Var varterm)) then (Var (varterm ++ encodingSuffix)) else (Var varterm)
enc (Constructor c interms outterms) = if (c == "cast") 
										then error "ERROR: Source type system checks for casts. The source type system must be of the original cast free calculus" 
										else (Constructor c (map enc interms) (map enc outterms))
enc (Application term1 term2) = (Application (enc term1) (enc term2))
enc (Bound x) = (Bound x)
enc (Encode term1 term2) = error "ERROR: Source type system contains Encoded terms"


withCasts :: CastSummary -> Term -> Term
withCasts castSummary (Var varterm) = let tmp = (HM.lookup (Var varterm) castSummary) in 
										case tmp of
										Just type1 -> if (isOutputType (Var varterm)) then (finalType castSummary (Var varterm)) else 
													  if (isTermExtended (Var varterm)) then (castWrap (Var varterm) type1 (finalType castSummary type1)) else (Var varterm)
										Nothing -> if (isTerm (Var varterm)) then error "ERROR: Encoding contains a term variables for which I cannot find a cast" else (Var varterm)
withCasts castSummary (Constructor c interms outterms) = 
									if (c == "cast") 
									then error "ERROR: Source term contains casts. The source term must be in the original cast free calculus" 
									else (Constructor c (map (withCasts castSummary) interms) (map (withCasts castSummary) outterms))
withCasts castSummary (Application term1 term2) = (Application (withCasts castSummary term1) (withCasts castSummary term2))
withCasts castSummary (Bound x) = (Bound x)

joinToFlow :: [Term] -> Term -> [Premise]
joinToFlow [] jointype = []
joinToFlow (typ:rest) jointype = (Formula "consistency" [] [] [typ,jointype]):(joinToFlow rest jointype)

finalType :: (HM.Map Term Term) -> Term -> Term
finalType typesToCast (Var variable) = let tmp = (HM.lookup (Var variable) typesToCast) in 
										case tmp of 
											Just type2 -> finalType typesToCast type2
											Nothing -> (Var variable)
finalType typesToCast (Constructor c interms outterms) = (Constructor c (map (finalType typesToCast) interms) (map (finalType typesToCast) outterms))

castWrap :: Term -> Term -> Term -> Term 
castWrap (Var variable) source target = if source == target then (Var variable) else case (isAbstraction (Var variable)) of 
										False -> cast (Var variable)
										True -> (Lambda "x" (cast (Application (Var variable) (Bound "x"))))
										where 
										cast = \x -> (Constructor "cast" [x, source] [(Var newLabel), target])
										newLabel = (unsafePerformIO gensymLabel)


