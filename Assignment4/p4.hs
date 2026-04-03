{-# LANGUAGE GADTs,FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Abstract Syntax Definitions
data KULang where
    Num :: Int -> KULang  
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang
    Mult :: KULang -> KULang -> KULang 
    Div :: KULang -> KULang -> KULang  
    Exp :: KULang -> KULang -> KULang
    If0 :: KULang -> KULang -> KULang -> KULang
    Id :: String -> KULang
    Lambda :: String -> KULang -> KULang 
    App :: KULang -> KULang -> KULang 
    deriving (Show,Eq)

data KULangVal where
    NumV :: Int -> KULangVal
    ClosureV :: String -> KULang -> EnvVal -> KULangVal
    deriving (Show,Eq)

data KULangExt where
    NumX :: Int -> KULangExt
    PlusX :: KULangExt -> KULangExt -> KULangExt
    MinusX :: KULangExt -> KULangExt -> KULangExt
    MultX :: KULangExt -> KULangExt -> KULangExt
    DivX :: KULangExt -> KULangExt -> KULangExt
    ExpX :: KULangExt -> KULangExt -> KULangExt
    If0X :: KULangExt -> KULangExt -> KULangExt -> KULangExt
    LambdaX :: String -> KULangExt -> KULangExt
    AppX :: KULangExt -> KULangExt -> KULangExt 
    BindX :: String -> KULangExt -> KULangExt -> KULangExt
    IdX :: String -> KULangExt
    deriving (Show,Eq)

-- Environment Definitions
type Env = [(String,KULang)]
type EnvVal = [(String,KULangVal)]

-- Reader Definition
data Reader e a = Reader (e -> Maybe a)

-- Monad Definition
instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
            Nothing -> Nothing
            Just x -> runR (f x) e

 -- Applicative Definition
instance Applicative (Reader e) where
pure x = Reader $ \e -> Just x
(Reader f) <*> (Reader g) = Reader $ \e -> case f e of
  Nothing -> Nothing
  Just fx -> case g e of
    Nothing -> Nothing
    Just gx -> Just (fx gx)

-- Functor Definition
instance Functor (Reader e) where
 fmap f (Reader g) = Reader $ \e -> case g e of
  Nothing -> Nothing
  Just x -> Just (f x)

-- Fail Definition
instance MonadFail (Reader e) where
    fail _ = Reader $ \_ -> Nothing

-- Helper Methods
runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

ask :: Reader a a 
ask = Reader $ \e -> Just e

local :: (e->t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> (runR r (f e))

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e 


-----------------------------
----- Project Exercises -----
-----------------------------

-- Part 1: Scoping

-- Exercise 1:
evalDyn :: Env -> KULang -> (Maybe KULang)
evalDyn _ _ = Nothing

-- Exercise 2:
evalStat :: EnvVal -> KULang -> (Maybe KULangVal)
evalStat _ _ = Nothing

-- Part 2: Elaboration

-- Exercise 3:
elabTerm :: KULangExt -> KULang 
elabTerm _ = (Num (-1))

-- Exercise 4:
interpElab :: EnvVal -> KULangExt -> (Maybe KULangVal)
interpElab _ _ = Nothing

-- Part 3: Reader Monad

-- Exercise 5:
evalReader :: KULang -> Reader EnvVal KULangVal
evalReader _ = undefined

-- Exercise 6:
interpReader :: KULangExt -> Maybe KULangVal
interpReader _ = undefined