{-# LANGUAGE GADTs,FlexibleContexts #-}
import GHC.Base (bindIO)


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
subst :: String -> KULang -> KULang -> KULang
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i' --if same id is recasted, then don't change the inner id
	                        then (Bind i' (subst i v v') b')
	                        else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i'
	                then v
	                else (Id i')

evalDirect :: KULang -> (Maybe KULang)
evalDirect (Boolean b) = Just (Boolean b)
evalDirect (Num x) = if x < 0 then Nothing else Just (Num x) --force the type to be num
evalDirect (Plus l r) = do {
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    Just (Num (l'+r'))
}
evalDirect (Minus l r ) = do {
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    (Num d) <- Just (Num(l'-r')); --I think I'm supposed to force d to be num type
    if d < 0 then Nothing else Just (Num d) --then do it again?
}
evalDirect (Mult l r) = do {
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    Just (Num (l'*r'))
}
evalDirect (Div l r ) = do {
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    if r' == 0 then Nothing else Just (Num (l' `div` r'))
}
evalDirect (Exp l r) = do{
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    Just (Num (l'^r'))   
}
evalDirect (And l r) = do{
    (Boolean l') <- evalDirect l;
    (Boolean r') <- evalDirect r;
    Just (Boolean (l'&&r'))
}
evalDirect (Or l r) = do {
    (Boolean l') <- evalDirect l;
    (Boolean r') <- evalDirect r;
    Just (Boolean (l' || r'))
}
evalDirect (Leq l r) = do {
    (Num l') <- evalDirect l;
    (Num r') <- evalDirect r;
    Just (Boolean (l' <= r'))
}
evalDirect (IsZero x) = do {
    (Num x') <- evalDirect x;
    Just (Boolean (x' == 0))
}
evalDirect (If c t e) = do{
    (Boolean c') <- evalDirect c;
    if c' then (evalDirect t) else (evalDirect e)
}
evalDirect(Between x y z) = do {
    (Num x') <- evalDirect x;
    (Num y') <- evalDirect y;
    (Num z') <- evalDirect z;
    evalDirect (And (Boolean(x' < y'))(Boolean(y' < z'))) --oh my god im awesome
}
evalDirect (Bind i v b) = do {v' <- evalDirect v;
    evalDirect (subst i v' b)
}
evalDirect (Id i) = Nothing
evalDirect _ = Nothing

-- Exercise 2
evalDeferred :: Env -> KULang -> (Maybe KULang)
evalDeferred e (Boolean b) = Just (Boolean b)
evalDeferred e (Num n) = return (Num n)
evalDeferred e (Plus l r) = do {(Num l') <- evalDeferred e l;
                        (Num r') <- evalDeferred e r;
                        return (Num (l'+r'))}
evalDeferred e (Minus l r ) = do {
    (Num l') <- evalDeferred e l;
    (Num r') <- evalDeferred e r;
    (Num d) <- Just (Num(l'-r')); 
    if d < 0 then Nothing else Just (Num d) 
}
evalDeferred e (Mult l r) = do {
    (Num l') <- evalDeferred e l;
    (Num r') <- evalDeferred e r;
    Just (Num (l'*r'))
}
evalDeferred e (Div l r ) = do {
    (Num l') <- evalDeferred e l;
    (Num r') <- evalDeferred e r;
    if r' == 0 then Nothing else Just (Num (l' `div` r'))
}
evalDeferred e (Exp l r) = do{
    (Num l') <- evalDeferred e l;
    (Num r') <- evalDeferred e r;
    Just (Num (l'^r'))   
}
evalDeferred e (And l r) = do{
    (Boolean l') <- evalDeferred e l;
    (Boolean r') <- evalDeferred e r;
    Just (Boolean (l'&&r'))
}
evalDeferred e (Or l r) = do {
    (Boolean l') <- evalDeferred e l;
    (Boolean r') <- evalDeferred e r;
    Just (Boolean (l' || r'))
}
evalDeferred e (Leq l r) = do {
    (Num l') <- evalDeferred e l;
    (Num r') <- evalDeferred e r;
    Just (Boolean (l' <= r'))
}
evalDeferred e (IsZero x) = do {
    (Num x') <- evalDeferred e x;
    Just (Boolean (x' == 0))
}
evalDeferred e (If c t h) = do{
    (Boolean c') <- evalDeferred e c;
    if c' then (evalDeferred e t) else (evalDeferred e h)
}
evalDeferred e (Between x y z) = do {
    (Num x') <- evalDeferred e x;
    (Num y') <- evalDeferred e y;
    (Num z') <- evalDeferred e z;
    evalDeferred e (And (Boolean(x' < y'))(Boolean(y' < z'))) --oh my god im awesome
}
evalDeferred e (Bind i v b) = do {
    v' <- evalDeferred e v;
    evalDeferred ((i,v'): e) b
}
evalDeferred e (Id i) = (lookup i e) --now we use the env to find our value
evalDeferred _ _ = Nothing

-- Exercise 3
testEvals :: KULang -> Bool
testEvals x = 
    if evalDirect x == evalDeferred [] x then True else False

testEvals _ = True

-- Part 2: Type Checking

--Exercise 1
typeofMonad :: Cont -> KULang -> (Maybe KUTypeLang)
typeofMonad c (Num x) = if x < 0 then Nothing else return TNum

typeofMonad c (Boolean x) = return TBool
typeofMonad c (Plus l r ) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r;
    return TNum
}
typeofMonad c (Minus l r) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r;
    return TNum  
}
typeofMonad c (Mult l r) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r;
    return TNum  
}
typeofMonad c (Div l r) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r; 
    return TNum  
}
typeofMonad c (Exp l r) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r;
    return TNum  
}
typeofMonad c (And l r) = do {
    TBool <- typeofMonad c l;
    TBool <- typeofMonad c r;
    return TBool  
}
typeofMonad c (Or l r) = do {
    TBool <- typeofMonad c l;
    TBool <- typeofMonad c r;
    return TBool  
}
typeofMonad c (Leq l r) = do {
    TNum <- typeofMonad c l;
    TNum <- typeofMonad c r;
    return TBool  
}
typeofMonad c (IsZero n) = do {
    TNum <- typeofMonad c n;
    return TBool
}
typeofMonad c' (If c t e) = do{
    TBool <- typeofMonad c' c;
    t' <- typeofMonad c' t;
    e' <- typeofMonad c' e;
    if t'==e' then return t' else Nothing
}
typeofMonad c (Between x y z) = do {
    TNum <- typeofMonad c x;
    TNum <- typeofMonad c y;
    TNum <- typeofMonad c z;
    return TBool  
}

typeofMonad c (Bind i v b) = do {
    v' <- typeofMonad c v;
    typeofMonad ((i,v'): c) b
}

typeofMonad c (Id i) = (lookup i c)
typeofMonad _ _ = Nothing

--Exercise 2
interpret :: KULang -> (Maybe KULang)
interpret x = do{
    _ <- typeofMonad [] x; --if this runs then we in the type clear
    evalDirect x; --return this value
}
interpret _ = Nothing

