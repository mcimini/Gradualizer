module InputTypes where

hasInputTypes :: String -> Bool
hasInputTypes "arrow" = True
hasInputTypes "refType" = True
hasInputTypes others = False