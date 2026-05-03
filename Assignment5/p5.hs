{-# LANGUAGE GADTs, PostfixOperators #-}

import Control.Monad

data KUTypeLang where
  TNum :: KUTypeLang
  TBool :: KUTypeLang
  (:->:) :: KUTypeLang -> KUTypeLang -> KUTypeLang
  TClosure :: String -> KUTypeLang -> Cont -> KUTypeLang
  deriving (Show,Eq)

data KULang where
  Num :: Int -> KULang
  Boolean :: Bool -> KULang
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
  Leq :: KULang -> KULang -> KULang
  IsZero :: KULang -> KULang
  Fix :: KULang -> KULang --fix t, should only take one input
  deriving (Show,Eq)  

data KULangExt where
  NumX :: Int -> KULangExt
  BooleanX :: Bool -> KULangExt
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
  LeqX :: KULangExt -> KULangExt -> KULangExt
  IsZeroX :: KULangExt -> KULangExt
  deriving (Show,Eq)

data KULangVal where
  NumV :: Int -> KULangVal
  BooleanV :: Bool -> KULangVal
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

-- ========== Project Exercises ========== --

-- Part 1 - Type Inference
typeof :: KULang -> Reader Cont KUTypeLang
typeof (Num n) = if n < 0 then error "fail" else return (TNum)
typeof (Boolean b) = return (TBool)
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
                         if a'==d then return r else error "fail"}
                     

typeof (If c t e') = do {
    TBool <- typeof c;
    typeT <- typeof t;
    typee <- typeof e';
    if typeT == typee then return typeT else error "fail"}

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

typeof (Leq x y) = do {
    TNum <- typeof x;
    TNum <- typeof y;
    return TBool
}

typeof (IsZero x) = do {
    TNum <- typeof x;
    return TNum
}
typeof (Fix f) = do {(d :->: r) <- typeof f;
                        if d == r then return d else error "fail"}
typeof _ = fail "Not implemented yet"
-- Part 2 - Evaluation
eval :: KULang -> Reader EnvVal KULangVal
eval (Num n) = if n<0 then error "fail" else return (NumV n)
eval (Boolean b) = return (BooleanV b)
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
    if d < 0 then error "fail" else return (NumV d)
}
eval (Mult l r) = do {
    (NumV l') <- eval l;
    (NumV r') <- eval r;
    return (NumV (l'*r'))
}
eval (Div l r ) = do {
    (NumV l') <- eval l;
    (NumV r') <- eval r;
    if r' == 0 then error "fail" else return (NumV (l' `div` r'))

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
    (NumV c') <- eval c;
    if c' == 0 then (eval t) else (eval e') }

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
eval (Leq x y) = do {
    (NumV x') <- eval x;
    (NumV y') <- eval y;
    return (BooleanV (x' <= y'))}

eval (IsZero x) = do {
    (NumV x') <- eval x;
    return (BooleanV (x' == 0))}   

eval (Fix f) = do
    (ClosureV i b env) <- eval f
    let rec_closure = ClosureV i b ((i, rec_closure) : env)
    local (useClosure i rec_closure env) (eval b)

eval _ = fail "Not implemented yet"



-- Part 3 - Add the Fixed Point Operator

-- Orginal code in the eval code block
-- eval (Fix f) = do
--     (ClosureV i b env) <- eval f
--     let rec_closure = ClosureV i b ((i, rec_closure) : env)
--     local (useClosure i rec_closure env) (eval b)

-- Orginal code in the typeof code block
-- typeof (Fix f) = do {(d :->: r) <- typeof f;
--     if d == r then return d else error "fail"}

-- Part 4 - Interpretation
interpret :: KULangExt -> Maybe KULangVal
interpret _ = Nothing

-- Test Cases

-- testFib = interpret ( 
--                 BindX "fib" (TNum :->: TNum)
--                     (FixX (LambdaX "g" (TNum :->: TNum)
--                         (LambdaX "x" TNum 
--                             (IfX (LeqX (IdX "x") (NumX 1))
--                                 (IdX "x")
--                                 (PlusX 
--                                     (AppX (IdX "g") (MinusX (IdX "x") (NumX 1)))
--                                     (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
--                                 )
--                             )
--                         )
--                     ))
--                     (AppX (IdX "fib") (NumX 2))) == 
--                 Just (NumV 1)

-- testIsEven = interpret ( 
--                 BindX "isEven" (TNum :->: TBool)
--                     (FixX (LambdaX "g" (TNum :->: TBool)
--                         (LambdaX "x" TNum
--                             (IfX (IsZeroX (IdX "x"))
--                                 (BooleanX True)
--                                 (IfX (LeqX (IdX "x") (NumX 1))
--                                     (BooleanX False)
--                                     (AppX (IdX "g") (MinusX (IdX "x") (NumX 2)))
--                                 )
--                             )
--                         )
--                     ))
--                     (AppX (IdX "isEven") (NumX 67))) == 
--                 Just (BooleanV False)