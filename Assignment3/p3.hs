{-# LANGUAGE GADTs,FlexibleContexts #-}


-- AST and Type Definitions

data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang  
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    Boolean :: Bool -> KULang  
    And :: KULang -> KULang -> KULang 
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang 
    Between :: KULang -> KULang -> KULang -> KULang
    Bind :: String -> KULang -> KULang -> KULang
    Id :: String -> KULang
    deriving (Show,Eq)


type Env = [(String,KULang)]

type Cont = [(String,KUTypeLang)]


-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Adding Booleans

-- Exercise 1
evalDirect :: KULang -> (Maybe KULang)
evalDirect _ = Nothing

-- Exercise 2
evalDeferred :: Env -> KULang -> (Maybe KULang)
evalDeferred _ _ = Nothing

-- Exercise 3
testEvals :: KULang -> Bool
testEvals _ = True

-- Part 2: Type Checking

--Exercise 1
typeofMonad :: Cont -> KULang -> (Maybe KUTypeLang)
typeofMonad _ _ = Nothing

--Exercise 2
interpret :: KULang -> (Maybe KULang)
interpret _ = Nothing

