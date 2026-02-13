{-# LANGUAGE GADTs, FlexibleContexts #-}


-- AST Definition
data KUTypeLang where
    TNum :: KUTypeLang
    TBool :: KUTypeLang 
    deriving (Show,Eq)

data KULang where
    Num :: Int -> KULang 
    Boolean :: Bool -> KULang
    Plus :: KULang -> KULang -> KULang 
    Minus :: KULang -> KULang -> KULang  
    Mult :: KULang -> KULang -> KULang  
    Div :: KULang -> KULang -> KULang   
    Exp :: KULang -> KULang -> KULang 
    And :: KULang -> KULang -> KULang   
    Or :: KULang -> KULang -> KULang  
    Leq :: KULang -> KULang -> KULang  
    IsZero :: KULang -> KULang  
    If :: KULang -> KULang -> KULang -> KULang  
    Between :: KULang -> KULang -> KULang -> KULang  
    deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Type Inference

-- Exercise 1
evalMonad :: KULang -> Maybe KULang
evalMonad (Boolean b) = Just (Boolean b)
evalMonad (Num x) = if x < 0 then Nothing else Just (Num x) --force the type to be num
evalMonad (Plus l r) = do {
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    Just (Num (l'+r'))
}
evalMonad (Minus l r ) = do {
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    (Num d) <- Just (Num(l'-r')); --I think I'm supposed to force d to be num type
    if d < 0 then Nothing else Just (Num d) --then do it again?
}
evalMonad (Mult l r) = do {
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    Just (Num (l'*r'))
}
evalMonad (Div l r ) = do {
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    if r' == 0 then Nothing else Just (Num (l' `div` r'))
}
evalMonad (Exp l r) = do{
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    Just (Num (l'^r'))   
}
evalMonad (And l r) = do{
    (Boolean l') <- evalMonad l;
    (Boolean r') <- evalMonad r;
    Just (Boolean (l'&&r'))
}
evalMonad (Or l r) = do {
    (Boolean l') <- evalMonad l;
    (Boolean r') <- evalMonad r;
    Just (Boolean (l' || r'))
}
evalMonad (Leq l r) = do {
    (Num l') <- evalMonad l;
    (Num r') <- evalMonad r;
    Just (Boolean (l' <= r'))
}
evalMonad (IsZero x) = do {
    (Num x') <- evalMonad x;
    Just (Boolean (x' == 0))
}
evalMonad (If c t e) = do {
    (Boolean c') <- evalMonad c;
    t' <- evalMonad t;
    e' <- evalMonad e;
    if c' then Just t' else Just e'
}
evalMonad(Between x y z) = do {
    (Num x') <- evalMonad x;
    (Num y') <- evalMonad y;
    (Num z') <- evalMonad z;
    evalMonad (And (Boolean(x' < y'))(Boolean(y' < z'))) --oh my god im awesome
}
evalMonad _ = Nothing

-- Exercise 2
typeofMonad :: KULang -> Maybe KUTypeLang
typeofMonad (Num x) = if x < 0 then Nothing else return TNum
typeofMonad (Boolean x) = return TBool
typeofMonad (Plus l r ) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r;
    return TNum
}
typeofMonad (Minus l r) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r;
    return TNum  
}
typeofMonad (Mult l r) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r;
    return TNum  
}
typeofMonad (Div l r) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r; --don't worry about 0 in type checking
    return TNum  
}
typeofMonad (Exp l r) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r;
    return TNum  
}
typeofMonad (And l r) = do {
    TBool <- typeofMonad l;
    TBool <- typeofMonad r;
    return TBool  
}
typeofMonad (Or l r) = do {
    TBool <- typeofMonad l;
    TBool <- typeofMonad r;
    return TBool  
}
typeofMonad (Leq l r) = do {
    TNum <- typeofMonad l;
    TNum <- typeofMonad r;
    return TBool  
}
typeofMonad (IsZero n) = do {
    TNum <- typeofMonad n;
    return TBool
}
typeofMonad (If c t e) = do{
    TBool <- typeofMonad c;
    t' <- typeofMonad t;
    e' <- typeofMonad e;
    if t'==e' then return t' else Nothing
}
typeofMonad (Between x y z) = do {
    TNum <- typeofMonad x;
    TNum <- typeofMonad y;
    TNum <- typeofMonad z;
    return TBool  
}
typeofMonad _ = Nothing

-- Exercise 3
interpTypeEval :: KULang -> Maybe KULang
interpTypeEval x = do{
    typeofMonad x; --if this runs then we in the type clear
    evalMonad x; --return this value
}
interpTypeEval _ = Nothing

-- Part 2: Optimizer

-- Exercise 1
optimize :: KULang -> KULang
optimize a = a

-- Exercise 2
interpOptEval :: KULang -> Maybe KULang
interpOptEval _ = Nothing
