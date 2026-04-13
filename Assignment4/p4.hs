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
evalDyn e (Num n) = return (Num n)
evalDyn e (Plus l r) = do {(Num l') <- evalDyn e l;
                        (Num r') <- evalDyn e r;
                        return (Num (l'+r'))}
evalDyn e (Minus l r ) = do {
    (Num l') <- evalDyn e l;
    (Num r') <- evalDyn e r;
    (Num d) <- Just (Num (l'-r'));
    if d < 0 then Nothing else Just (Num d)
}
evalDyn e (Mult l r) = do {
    (Num l') <- evalDyn e l;
    (Num r') <- evalDyn e r;
    Just (Num (l'*r'))
}
evalDyn e (Div l r ) = do {
    (Num l') <- evalDyn e l;
    (Num r') <- evalDyn e r;
    if r' == 0 then Nothing else Just (Num (l' `div` r'))
}
evalDyn e (Exp l r) = do {
     (Num l') <- evalDyn e l;
     (Num r') <- evalDyn e r;
     Just (Num (l'^r'))
 }
evalDyn e (If0 c t e') = do {
    (Num c') <- evalDyn e c;
    if c' == 0 then (evalDyn e t) else (evalDyn e e')
}
evalDyn e (Id i) = (lookup i e) --now we use the env to find our value
evalDyn e (Lambda i b) = return (Lambda i b)
evalDyn e (App f a) = do {(Lambda i b) <- evalDyn e f;
                        v <- evalDyn e a;
                        evalDyn ((i,v):e) b}
evalDyn _ _ = Nothing

-- Exercise 2:
evalStat :: EnvVal -> KULang -> (Maybe KULangVal)
evalStat e (Num n) = return (NumV n)
evalStat e (Plus l r) = do {(NumV l') <- evalStat e l;
                        (NumV r') <- evalStat e r;
                        return (NumV (l'+r'))}
evalStat e (Minus l r ) = do {
    (NumV l') <- evalStat e l;
    (NumV r') <- evalStat e r;
    (NumV d) <- Just (NumV (l'-r'));
    if d < 0 then Nothing else Just (NumV d)
}
evalStat e (Mult l r) = do {
    (NumV l') <- evalStat e l;
    (NumV r') <- evalStat e r;
    Just (NumV (l'*r'))
}
evalStat e (Div l r ) = do {
    (NumV l') <- evalStat e l;
    (NumV r') <- evalStat e r;
    if r' == 0 then Nothing else Just (NumV (l' `div` r'))
}
evalStat e (Exp l r) = do {
     (NumV l') <- evalStat e l;
     (NumV r') <- evalStat e r;
     Just (NumV (l'^r'))
 }
evalStat e (If0 c t e') = do {
    (NumV c') <- evalStat e c;
    if c' == 0 then (evalStat e t) else (evalStat e e')
}
evalStat e (Id i) = (lookup i e) --now we use the env to find our value
evalStat e (Lambda i b) = return (ClosureV i b e)
evalStat e (App f a) = do {(ClosureV i b ce)<- evalStat e f;
                        v <- evalStat e a;
                        evalStat ((i,v):ce) b}
evalStat _ _ = Nothing

-- Part 2: Elaboration

-- Exercise 3:
elabTerm :: KULangExt -> KULang
elabTerm (NumX n) = Num n
elabTerm (PlusX l r) = Plus (elabTerm l) (elabTerm r)
elabTerm (MinusX l r) = Minus (elabTerm l) (elabTerm r)
elabTerm (MultX l r) = Mult (elabTerm l) (elabTerm r)
elabTerm (DivX l r) = Div (elabTerm l) (elabTerm r)
elabTerm (ExpX l r) = Exp (elabTerm l) (elabTerm r)
elabTerm (If0X c t e') = If0 (elabTerm c) (elabTerm t) (elabTerm e')
elabTerm (LambdaX i b) = Lambda i (elabTerm b)
elabTerm (AppX f a) = App (elabTerm f) (elabTerm a)
elabTerm (BindX i v b) = App (Lambda i (elabTerm b)) (elabTerm v)
elabTerm (IdX i) = Id i
elabTerm _ = (Num (-1))

-- Exercise 4:
interpElab :: EnvVal -> KULangExt -> (Maybe KULangVal)
interpElab e t = evalStat e (elabTerm t)
interpElab _ _ = Nothing

-- Part 3: Reader Monad

-- Exercise 5:
evalReader :: KULang -> Reader EnvVal KULangVal
evalReader (Num n) = if n<0 then error "fail" else return (NumV n)

evalReader (Plus l r) = do {(NumV l') <- evalReader l;
                      (NumV r') <- evalReader r;
                      return (NumV (l'+ r'))}

evalReader (Minus l r ) = do {
    (NumV l') <- evalReader l;
    (NumV r') <- evalReader r;
    let d = l' - r' in
    if d < 0 then error "fail" else return (NumV d)
}
evalReader (Mult l r) = do {
    (NumV l') <- evalReader l;
    (NumV r') <- evalReader r;
    return (NumV (l'*r'))
}
evalReader (Div l r ) = do {
    (NumV l') <- evalReader l;
    (NumV r') <- evalReader r;
    if r' == 0 then error "fail" else return (NumV (l' `div` r'))
}
evalReader (Exp l r) = do {
     (NumV l') <- evalReader l;
     (NumV r') <- evalReader r;
     return (NumV (l'^r'))
 }
evalReader (If0 c t e') = do {
    (NumV c') <- evalReader c;
    if c' == 0 then (evalReader t) else (evalReader e') }

evalReader (Lambda i b) = do {env <- ask;
                    return (ClosureV i b env)}

evalReader (Id i) = do {env <- ask;
                    case (lookup i env) of
                    Just x -> return x
                    Nothing -> fail "unbound variable"}
evalReader (App f a) = do {
                     (ClosureV i b e) <- evalReader f;
                     v <- evalReader a;
                     local (useClosure i v e) (evalReader b)}
evalReader _ = undefined

-- Exercise 6:
interpReader :: KULangExt -> Maybe KULangVal
interpReader x = runR (evalReader (elabTerm x)) []
interpReader _ = undefined