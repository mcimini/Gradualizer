module GenerateCalculi where

import System.IO
import System.IO.Unsafe
import Data.Unique
import Data.List
import TypeSystem
import PatternMatching
import FlowDiscovery
import Static
import TypeSystemForCC
import CastInsertion
import GenerateAuxiliaryPredicates
import FromLambdaProlog
import CastManagement
import ToLambdaProlog
import Library

generate :: IO ()
generate = do
   mapM_ (gradualize "All" LazyDTwo) (tail preLibrary)
   return ()

gradualize :: String -> CastStrategy -> String -> IO ()
gradualize choice strategy name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod") 
               let signature = lines streamSig
               let moduleL = lines streamMod
               let ts = parseLP (signature ++ moduleL)
               let targetTs = (generateRules choice strategy ts)
               let targetSig = (generateSigOnlyNew choice strategy targetTs ts)
               let gradualName = "gradual_" ++ name
               let sigPreamble = "sig " ++ gradualName ++ ".\n\n"
               let modPreamble = "module " ++ gradualName ++ ".\n\n"
               writeFile ("Gradualized/" ++ gradualName ++ ".sig") (sigPreamble ++ (unlines (drop 1 signature)) ++ "\n\n" ++ (toLambdaPrologSig choice targetSig))
               writeFile ("Gradualized/" ++ gradualName ++ ".mod") (modPreamble ++ (unlines (drop 1 moduleL)) ++ "\n\n" ++ (toLambdaPrologModule targetTs))

generateRules :: String -> CastStrategy -> TypeSystem -> TypeSystem
generateRules mode strategy (Ts sig rules) = let ts = (Ts sig (filter (filterRules typeOf) rules)) in case mode of
         "Original" -> ts
         "Gradual" -> extend (toGradual ts) (auxiliaryPred (toGradual ts))
         "GradualOnly" -> (toGradual ts) 
         "CC" -> extend (extend (extend (toTypeSystemForCC ts) (castInsertion ts)) (castManagement strategy (toTypeSystemForCC ts))) (auxiliaryPred (toGradual ts))
         "CI" -> (castInsertion ts)
         "Aux" -> auxiliaryPred (toGradual ts)
         "Dynamic" -> (castManagement strategy (toTypeSystemForCC ts))
         _ -> (extend (generateRules "GradualOnly" strategy ts) (generateRules "CC" strategy ts)) 

generateSigOnlyNew :: String -> CastStrategy -> TypeSystem -> TypeSystem -> Signature
generateSigOnlyNew mode strategy ts tsOriginal = case mode of
         "Gradual" -> sig_dyntype ++ sig_typeOfGr ++ (sigOfNewThings ts)
         "CC" -> sig_dyntype ++ sig_castCalculus ++ sig_typeOfCI ++ sigOfdynamic ++ (sigOfNewThings ts) ++ stepTicked_ifNecessary ts
         _ -> nub (sig_dyntype ++ sig_typeOfGr ++ sig_castCalculus ++ sig_typeOfCI ++ (sigOfNewThings ts) ++ sigOfdynamic ++ stepTicked_ifNecessary ts)

stepTicked_ifNecessary ts = if doesItAppear "step'" ts then [Decl "step'" "o" Nothing Nothing [(Simple "term"), (Simple "term")]] else []

