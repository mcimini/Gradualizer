module InsertJoin where

import qualified Data.Map as HM
import Data.List
import Data.String.Utils
import TypeSystem

type Equated = (HM.Map Term [Term])

joinPrefix :: String
joinPrefix = "Join"

insertJoin :: Rule -> Rule
insertJoin (Rule premises conclusion) = (Rule (premises ++ joinPremises) conclusion) where joinPremises = map insertJoin_term (HM.toList (extractEquated premises HM.empty))

insertJoin_term :: (Term,[Term]) -> Premise
insertJoin_term (joinVar,vars) = (Formula "join" [(show (length vars))] vars [joinVar])

extractEquated :: [Premise] -> Equated -> Equated
extractEquated [] equated = equated
extractEquated (premise:rest) equated = (extractEquated rest equated')
               where
               equated' = case premise of
                     (Formula "flow" strings interms outterms) -> if isJoinVar (interms !! 1) then HM.insertWith (++) (interms !! 1) [(head interms)] equated  else equated
                     otherwise -> equated


makeJoinVar :: Term -> Term
makeJoinVar (Var variable) = (Var (joinPrefix ++ variable))

isJoinVar :: Term -> Bool
isJoinVar (Var variable) = startswith joinPrefix variable