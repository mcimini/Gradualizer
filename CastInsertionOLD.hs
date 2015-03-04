module ToCastInsertion where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import PatternMatching
import UpToConsistency
import ResolutionAndStatic

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

thd3 :: (a, b, c) -> c
thd3 (_,_,x) = x


gensymLabel :: IO String
gensymLabel = do 
			sym <- newUnique
			return ("L" ++ show (hashUnique sym))

encodingSuffix :: String
encodingSuffix = "'"


toCastInsertion :: TypeSystem -> TypeSystem
toCastInsertion ts = toCastInsertion_ (toGradual ts)

toCastInsertion_ :: TypeSystem -> TypeSystem
toCastInsertion_ (Ts sig rules) = (Ts sig (map toCastInsertionR rules))

						
toCastInsertionR :: Rule -> Rule 
toCastInsertionR (Rule premises term typ) = (Rule premises' (Encode term (withCasts termsToCast typesToCast term)) typ) where 
											result = map toCastInsertionPr premises
											premises' = (map fst3 result)
											termsToCast = (foldl HM.union HM.empty (map snd3 result))
											typesToCast = (foldl HM.union HM.empty (map thd3 result)) 

toCastInsertionPr :: Premise -> (Premise, (HM.Map Term Term), (HM.Map Term Term))
toCastInsertionPr (Formula pred strings interms outterms) = 
						case pred of 
							"typeOf" -> ((Formula "compToCC" [] [(Encode (head interms)] [(replaceEncoded (head interms)))] outterms),
										(HM.insert (head interms) (head outterms) $ HM.empty), HM.empty )
							"join" -> ((Formula pred strings interms outterms), HM.empty, typesToCast ) where										
											flowpremises = (joinToFlow interms (head outterms))
											typesToCast = (foldl1 HM.union (map thd3 (map toCastInsertionPr flowpremises)))
							"match" -> ((Formula pred strings interms outterms), HM.empty, (HM.insert (head interms) (Constructor (head strings) outterms) $ HM.empty))
							"consistency" -> ((Formula pred strings interms outterms), HM.empty, (HM.insert (head interms) (last interms) $ HM.empty) )
							otherwise -> ((Formula pred strings interms outterms), HM.empty, HM.empty )
toCastInsertionPr (Hypothetical bound outterm1 interm1 interm2 outterm2) = ((Hypothetical bound (Encode outterm1 (replaceEncoded outterm1)) interm1 (Encode interm2 (replaceEncoded interm2)) outterm2),
												(HM.insert interm2 outterm2 $ HM.empty), HM.empty )
											
replaceEncoded :: Term -> Term
replaceEncoded (Var varterm) = (Var (varterm ++ encodingSuffix))
replaceEncoded (Constructor c interms outterms) = if (c == "cast") 
										then error "ERROR: Source type system checks for casts. The source type system must be of the original cast free calculus" 
										else (Constructor c (map replaceEncoded interms) (map replaceEncoded outterms))
replaceEncoded (Application term1 term2) = (Application (replaceEncoded term1) (replaceEncoded term2))
replaceEncoded (Bound x) = (Bound x)
replaceEncoded (Encode term1 term2) = error "ERROR: Source type system contains Encoded terms"

withCasts :: (HM.Map Term Term) -> (HM.Map Term Term) -> Term -> Term
withCasts termsToCast typesToCast (Var varterm) = let tmp = (HM.lookup (Var varterm) termsToCast) in 
													 let encodedvar = (Var (varterm ++ encodingSuffix)) in 
														case tmp of 
															Just type1 -> (let tmp' = (HM.lookup type1 typesToCast) in 
																(case tmp' of
																	Just type2 -> (Constructor "cast" [type1,typeFinal2, (Var (unsafePerformIO gensymLabel)),encodedvar])
																	 where 
																		typeFinal1 =  finalType typesToCast type1
																		typeFinal2 =  finalType typesToCast type2
												  					Nothing -> encodedvar))
															Nothing -> if (any (isVarPresent (Var varterm)) (HM.keys termsToCast)) then encodedvar else (Var varterm)
withCasts termsToCast typesToCast (Constructor c interms outterms) = 
				if (c == "cast") 
				then error "ERROR: Source term contains casts. The source term must be in the original cast free calculus" 
				else (Constructor c (map (withCasts termsToCast typesToCast) interms) (map (withCasts termsToCast typesToCast) outterms))
withCasts termsToCast typesToCast (Application term1 term2) = (Application (withCasts termsToCast typesToCast term1) (withCasts termsToCast typesToCast term2))
withCasts termsToCast typesToCast (Bound x) = (Bound x)

joinToFlow :: [Term] -> Term -> [Premise]
joinToFlow [] jointype = []
joinToFlow (typ:rest) jointype = (Formula "consistency" [] [] [typ,jointype]):(joinToFlow rest jointype)

finalType :: (HM.Map Term Term) -> Term -> Term
finalType typesToCast (Var variable) = let tmp = (HM.lookup (Var variable) typesToCast) in 
										case tmp of 
											Just type2 -> finalType typesToCast type2
											Nothing -> (Var variable)
finalType typesToCast (Constructor c terms) = (Constructor c (map (finalType typesToCast) terms))

