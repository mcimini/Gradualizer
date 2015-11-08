module Static where

import System.IO.Unsafe 
import Data.Unique
import qualified Data.Map as HM
import Data.List
import TypeSystem
import FinalType
import FlowDiscovery 

sig_dyntype :: [SignatureEntry]
sig_dyntype = [Decl "dyn" "typ" Nothing Nothing []]

listOfTypesMinusDyn :: Signature -> Signature
listOfTypesMinusDyn sig = delete (head sig_dyntype) (listOfTypes sig)

sig_typeOfGr :: Signature
sig_typeOfGr = [Decl typeOfGradual "o" Nothing Nothing [Simple "term", Simple "typ"]]

sig_static :: Signature
sig_static = [Decl "static" "o" Nothing Nothing [Simple "typ"]]

toGradual :: TypeSystem -> TypeSystem
toGradual ts = (Ts (sig ++ sig_dyntype) rules')
              where
              (Ts sig rules) = (flowDiscovery ts)
              rules' = (map (renamingInRule typeOf typeOfGradual) (map addStatic rules))

addStatic :: Rule -> Rule
addStatic (Rule premises conclusion) = (Rule (premises ++ staticpremises) conclusion)
               where
               staticpremises = map staticWrapper (filter (notIn (outputsOrInputs "outputs" (Rule premises conclusion))) (outputsOrInputs "inputs" (Rule premises conclusion)))
               notIn = \outputs -> \variable -> not (elem variable outputs)

staticWrapper :: Term -> Premise
staticWrapper = \variable -> (Formula "static" [] [variable] [])
