module Test where


import           System.IO
import           System.Process
import           System.Directory
import           Control.Monad
import           TypeSystem
import           GenerateCalculi
import           FromLambdaProlog
import           ToLambdaProlog
import           Library
import           ToLatex

test :: IO ()
test = do
   mapM_ unitTest (tail preLibrary)
   mapM_ typeability (tail preLibrary)
   return ()
   
unitTest :: String -> IO ()
unitTest systemName = do
   gradualize "All" LazyDTwo systemName

typeability :: String -> IO ()
typeability systemName = do
    old <- getCurrentDirectory
    setCurrentDirectory "Gradualized/"
    mapM_ unitTypeTest (tail preLibrary)
    setCurrentDirectory old

unitTypeTest :: String -> IO ()
unitTypeTest systemName = do
    let source = "gradual_" ++ systemName
    callCommand ("tjcc " ++ source ++ " > " ++ systemName ++ ".log")

readFileAndShow :: IO [String]
readFileAndShow = do
       mapM readFileAndShow_ (tail preLibrary)
       
readFileAndShow_ :: String -> IO String
readFileAndShow_ name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod")
               return (":BEGIN SIG:" ++ name ++ "::\n" ++ streamSig ++ "\n\n:BEGIN MOD:" ++ streamMod)
--               let signature = lines streamSig
--               let moduleL = lines streamMod
--               let ts = parseLP (signature ++ moduleL)
--               let fileForTs = "typesystem_" ++ name
--               writeFile ("Gradualized/" ++ fileForTs) (show ts)

parseAndSpitTS_ :: String -> IO TypeSystem
parseAndSpitTS_ name = do
               streamSig <- readFile ("Repo of Static Type Systems/" ++ name ++ ".sig") 
               streamMod <- readFile ("Repo of Static Type Systems/" ++ name ++ ".mod")
               let signature = lines streamSig
               let moduleL = lines streamMod
               let ts = parseLP (signature ++ moduleL)
               return ts
