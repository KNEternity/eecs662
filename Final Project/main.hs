{-# LANGUAGE GADTs, PostfixOperators #-}

import Control.Monad
import Data.Text.Array (run)

data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  TStr :: KUTypeLang
  (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang
  TClosure :: String -> KUTypeLang -> Cont -> KUTypeLang
  deriving (Show,Eq)

data KULang where
  Num :: Int -> KULang
  Boolean :: Bool -> KULang
  Str :: String -> KULang
  Id :: String -> KULang
  Plus :: KULang -> KULang -> KULang
  Minus :: KULang -> KULang -> KULang
  Mult :: KULang -> KULang -> KULang
  Div :: KULang -> KULang -> KULang
  Exp :: KULang -> KULang -> KULang
  Between :: KULang -> KULang -> KULang -> KULang
  Lambda :: String -> KUTypeLang -> KULang -> KULang
  App :: KULang -> KULang -> KULang
  If :: KULang -> KULang -> KULang -> KULang
  And :: KULang -> KULang -> KULang
  Or :: KULang -> KULang -> KULang
  Geq :: KULang -> KULang -> KULang
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  Fix :: KULang -> KULang --fix t, should only take one input
  Concat :: KULang -> KULang -> KULang
  ReverseStr :: KULang -> KULang
  deriving (Show,Eq)

data KULangExt where
  NumX :: Int -> KULangExt
  BooleanX :: Bool -> KULangExt
  StrX :: String -> KULangExt
  IdX :: String -> KULangExt  
  PlusX :: KULangExt -> KULangExt -> KULangExt
  MinusX :: KULangExt -> KULangExt -> KULangExt
  MultX :: KULangExt -> KULangExt -> KULangExt
  DivX :: KULangExt -> KULangExt -> KULangExt
  ExpX :: KULangExt -> KULangExt -> KULangExt
  BetweenX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  LambdaX :: String -> KUTypeLang -> KULangExt -> KULangExt
  AppX :: KULangExt -> KULangExt -> KULangExt 
  BindX :: String -> KUTypeLang -> KULangExt -> KULangExt -> KULangExt
  IfX :: KULangExt -> KULangExt -> KULangExt -> KULangExt
  AndX :: KULangExt -> KULangExt -> KULangExt
  OrX :: KULangExt -> KULangExt -> KULangExt
  GeqX :: KULangExt -> KULangExt -> KULangExt
  LeqX :: KULangExt -> KULangExt -> KULangExt
  IsZeroX :: KULangExt -> KULangExt
  FixX :: KULangExt -> KULangExt
  ConcatX :: KULangExt -> KULangExt -> KULangExt
  ReverseStrX :: KULangExt -> KULangExt
  deriving (Show,Eq)

data KULangVal where
  NumV :: Int -> KULangVal
  BooleanV :: Bool -> KULangVal
  StrV :: String -> KULangVal
  ClosureV :: String -> KULang -> EnvVal -> KULangVal
  deriving (Show,Eq)

type EnvVal = [(String,KULangVal)]
type Cont = [(String,KUTypeLang)]

-- Reader & Helper Methods
data Reader e a = Reader (e -> Maybe a)

ask :: Reader a a 
ask = Reader $ \e -> Just e

runR :: Reader e a -> e -> Maybe a
runR (Reader f) e = f e 

local :: (e -> t) -> Reader t a -> Reader e a
local f r = Reader $ \e -> runR r (f e)

useClosure :: String -> KULangVal -> EnvVal -> EnvVal -> EnvVal
useClosure i v e _ = (i,v):e

-- Substitution helper
subst :: String -> KULang -> KULang -> KULang
subst i v (Id x)        = if x == i then v else Id x
subst i v (Plus l r)    = Plus (subst i v l) (subst i v r)
subst i v (Minus l r)   = Minus (subst i v l) (subst i v r)
subst i v (Mult l r)    = Mult (subst i v l) (subst i v r)
subst i v (Div l r)     = Div (subst i v l) (subst i v r)
subst i v (Exp l r)     = Exp (subst i v l) (subst i v r)
subst i v (Between x y z) = Between (subst i v x) (subst i v y) (subst i v z)
subst i v (Lambda x d b)  = if x == i then Lambda x d b
                             else Lambda x d (subst i v b)
subst i v (App f a)     = App (subst i v f) (subst i v a)
subst i v (If c t e')   = If (subst i v c) (subst i v t) (subst i v e')
subst i v (And x y)     = And (subst i v x) (subst i v y)
subst i v (Or x y)      = Or (subst i v x) (subst i v y)
subst i v (Geq x y)     = Geq (subst i v x) (subst i v y)
subst i v (Leq x y)     = Leq (subst i v x) (subst i v y)
subst i v (IsZero x)    = IsZero (subst i v x)
subst i v (Fix f)       = Fix (subst i v f)
subst i v (Concat l r) = Concat (subst i v l) (subst i v r)
subst i v (ReverseStr x) = ReverseStr (subst i v x)
subst _ _ e             = e  -- Num, Boolean base cases

instance Monad (Reader e) where
    g >>= f = Reader $ \e -> 
        case runR g e of
      Nothing -> Nothing
      Just v  -> runR (f v) e

instance Functor (Reader e) where
    fmap f (Reader g) = Reader $ \e ->
        case g e of
        Nothing -> Nothing
        Just v  -> Just (f v)

instance Applicative (Reader e) where
    pure x = Reader $ \e -> Just x
    (Reader f) <*> (Reader g) = Reader $ \e ->
        case f e of
      Nothing -> Nothing
      Just h  ->
        case g e of
          Nothing -> Nothing
          Just x  -> Just (h x)

instance MonadFail (Reader e) where
  fail _ = Reader $ \_ -> Nothing


-- Type Inference
typeof :: KULang -> Reader Cont KUTypeLang
typeof (Num n) = if n < 0 then error "Num typefail" else return (TNum)
typeof (Boolean b) = return (TBool)
typeof (Str s) = return (TStr) 
typeof (Id i) = do {cont <- ask;
                    case (lookup i cont) of
                    Just x -> return x
                    Nothing -> fail "unbound variable"}

typeof (Plus l r) = do {TNum <- typeof l;
                      TNum <- typeof r;
                      return TNum}

typeof (Minus l r ) = do {
    TNum <- typeof l;
    TNum <- typeof r;
    return TNum
}
typeof (Mult l r) = do {
    TNum <- typeof l;
    TNum <- typeof r;
    return (TNum)
}
typeof (Div l r ) = do {
    TNum <- typeof l;
    TNum <- typeof r;
    return (TNum)
}
typeof (Exp l r) = do {
     TNum <- typeof l;
     TNum <- typeof r;
     return (TNum)
 }

typeof (Between x y z) = do {
    TNum <- typeof x;
    TNum <- typeof y;
    TNum <- typeof z;
    return (TBool)
}

typeof (Lambda i d b) = do {
    r <- local ((i,d):) ( typeof b );
    return (d :->: r)}

typeof (App f a) = do {a' <- typeof a;
                         d :->: r <- typeof f;
                         if a'==d then return r else error "App type fail"}
                     

typeof (If c t e') = do {
    TBool <- typeof c;
    typeT <- typeof t;
    typee <- typeof e';
    if typeT == typee then return typeT else error "If type fail"}

typeof (And x y) = do {
    TBool <- typeof x;
    TBool <- typeof y;
    return TBool
}

typeof (Or x y) = do {
    TBool <- typeof x;
    TBool <- typeof y;
    return TBool
}

typeof (Geq x y) = do {
    TNum <- typeof x;
    TNum <- typeof y;
    return TBool
}

typeof (Leq x y) = do {
    TNum <- typeof x;
    TNum <- typeof y;
    return TBool
}

typeof (IsZero x) = do {
    TNum <- typeof x;
    return TBool
}
typeof (Fix f) = do {(d :->: r) <- typeof f;
    if d == r then return d else error "Fix type fail"}

typeof (Concat l r) = do {
    TStr <- typeof l;
    TStr <- typeof r;
    return TStr
}
typeof (ReverseStr x) = do {
    TStr <- typeof x;
    return TStr
}
typeof _ = fail "Not implemented yet"
-- Part 2 - Evaluation
eval :: KULang -> Reader EnvVal KULangVal
eval (Num n) = if n<0 then error "Num eval fail" else return (NumV n)
eval (Boolean b) = return (BooleanV b)
eval (Str s) = return (StrV s)
eval (Id i) = do {env <- ask;
                    case (lookup i env) of
                    Just x -> return x
                    Nothing -> fail "unbound variable"} 

eval (Plus l r) = do {(NumV l') <- eval l;
                      (NumV r') <- eval r;
                      return (NumV (l'+ r'))}

eval (Minus l r ) = do {
    (NumV l') <- eval l;
    (NumV r') <- eval r;
    let d = l' - r' in
    if d < 0 then error "Minus eval fail" else return (NumV d)
}
eval (Mult l r) = do {
    (NumV l') <- eval l;
    (NumV r') <- eval r;
    return (NumV (l'*r'))
}
eval (Div l r ) = do {
    (NumV l') <- eval l;
    (NumV r') <- eval r;
    if r' == 0 then error "Div eval fail" else return (NumV (l' `div` r'))

}

eval (Exp l r) = do {
     (NumV l') <- eval l;
     (NumV r') <- eval r;
     return (NumV (l'^r'))
 }

eval(Between x y z) = do {
    (NumV x') <- eval x;
    (NumV y') <- eval y;
    (NumV z') <- eval z;
    eval (And (Boolean(x' < y'))(Boolean(y' < z'))) 
}

eval (Lambda i d b) = do {env <- ask; --d stands for the domain type
                    return (ClosureV i b env)} --this is because we are static scoping

eval (App f a) = do {
                     (ClosureV i b e) <- eval f;
                     v <- eval a;
                     local (useClosure i v e) (eval b)}
eval (If c t e') = do {
    (BooleanV c') <- eval c;
    if c' then eval t else eval e' }

eval (And x y) = do {
    (BooleanV x') <- eval x;
    (BooleanV y') <- eval y;
    return (BooleanV (x' && y'))
}
eval (Or x y) = do {
    (BooleanV x') <- eval x;
    (BooleanV y') <- eval y;
    return (BooleanV (x' || y'))
}
eval (Geq x y) = do {
    (NumV x') <- eval x;
    (NumV y') <- eval y;
    return (BooleanV (x' >= y'))}

eval (Leq x y) = do {
    (NumV x') <- eval x;
    (NumV y') <- eval y;
    return (BooleanV (x' <= y'))}

eval (IsZero x) = do {
    (NumV x') <- eval x;
    return (BooleanV (x' == 0))}   

eval (Fix f) = do
    (ClosureV i b _) <- eval f
    eval (subst i (Fix (Lambda i TNum b)) b)

eval (Concat l r) = do {
    (StrV l') <- eval l;
    (StrV r') <- eval r;
    return (StrV (l' ++ r'))
}
eval (ReverseStr x) = do {
    (StrV x') <- eval x;
    return (StrV (reverse x'))
}

eval _ = fail "Not implemented yet"

--Elaborator that converts KULangExt to KULang
elabTerm :: KULangExt -> KULang
elabTerm (NumX n) = Num n
elabTerm (BooleanX b) = Boolean b
elabTerm (StrX s) = Str s
elabTerm (IdX id) = Id id
elabTerm (PlusX l r) = Plus (elabTerm l) (elabTerm r)
elabTerm (MinusX l r) = Minus (elabTerm l) (elabTerm r)
elabTerm (MultX l r) = Mult (elabTerm l) (elabTerm r)
elabTerm (DivX l r) = Div (elabTerm l) (elabTerm r)
elabTerm (ExpX l r) = Exp (elabTerm l) (elabTerm r)
elabTerm (BetweenX x y z) = Between (elabTerm x) (elabTerm y) (elabTerm z)
elabTerm (LambdaX i d b) = Lambda i d (elabTerm b)
elabTerm (AppX f a) = App (elabTerm f) (elabTerm a)
elabTerm (BindX i d v b) = App (Lambda i d (elabTerm b)) (elabTerm v)
elabTerm (IfX c t e') = If (elabTerm c) (elabTerm t) (elabTerm e')
elabTerm (AndX x y) = And (elabTerm x) (elabTerm y)
elabTerm (OrX x y) = Or (elabTerm x) (elabTerm y)
elabTerm (GeqX x y) = Geq (elabTerm x) (elabTerm y)
elabTerm (LeqX x y) = Leq (elabTerm x) (elabTerm y)
elabTerm (IsZeroX x) = IsZero (elabTerm x)
elabTerm (FixX f) = Fix (elabTerm f)
elabTerm (ConcatX l r) = Concat (elabTerm l) (elabTerm r)
elabTerm (ReverseStrX x) = ReverseStr (elabTerm x)
elabTerm _ = (Num (-1))

interpret :: KULangExt -> Maybe KULangVal
interpret t = do {
    runR (typeof (elabTerm t)) []; --run this to check type
    runR (eval (elabTerm t)) [] --run this to evaluate
}
interpret _ = Nothing