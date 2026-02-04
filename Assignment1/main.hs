{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Import Mapping
import Data.Map (Map)

-- Imports for Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Token 
import Data.Time.Format.ISO8601 (yearFormat)
import GHC.Bits (Bits(xor))

-- Abstract Syntax Definition
data KULang where
 Num :: Int -> KULang
 Plus :: KULang -> KULang -> KULang
 Minus :: KULang -> KULang -> KULang
 Mult :: KULang -> KULang -> KULang
 Div :: KULang -> KULang -> KULang
 Exp :: KULang -> KULang -> KULang
 deriving (Show,Eq)



-- Exercise 1
evalErr :: KULang -> Int --OOOOOH IM SUPPOSED TO IMPLEMENT NUM PLUS MINUS ETC
evalErr (Num x) = if x<0 then error "negative not allowed" else x
evalErr (Plus l r) = (evalErr l) + (evalErr r)
evalErr (Minus l r) = if (evalErr l) < (evalErr r) then error "No negative results allowed" else ((evalErr l) - (evalErr r))
evalErr (Mult l r) = (evalErr l) * (evalErr r)
evalErr (Div l r) = if (evalErr r) == 0 then error "Cannot divide by zero" else (evalErr l) `div` (evalErr r)
evalErr (Exp l r) = (evalErr l) ^ (evalErr r)

evalErr _ = error "Not recognized"

--Exercise 2
evalMaybe :: KULang -> Maybe Int 
evalMaybe (Num x) = if x<0 then Nothing else (Just x)
evalMaybe (Plus l r) = case (evalMaybe l) of 
                      Just l -> case (evalMaybe r) of
                        Just r -> Just (l+r)
                        Nothing -> Nothing
                      Nothing -> Nothing 
evalMaybe (Minus l r) = case (evalMaybe l) of
  Just l -> case (evalMaybe r) of 
    Just r -> if (l-r) < 0 
      then Nothing
      else Just (l-r)
    Nothing -> Nothing
  Nothing -> Nothing

evalMaybe (Mult l r) = case (evalMaybe l) of 
  Just l -> case (evalMaybe r) of 
    Just r -> Just (l*r)
    Nothing -> Nothing
  Nothing -> Nothing

evalMaybe (Div l r) = case (evalMaybe l) of 
  Just l -> case (evalMaybe r) of 
    Just r -> if r == 0 
      then Nothing
      else Just (l `div` r)
    Nothing -> Nothing
  Nothing -> Nothing

evalMaybe (Exp l r) = case (evalMaybe l) of 
  Just l -> case (evalMaybe r) of
    Just r -> Just (l ^ r)
    Nothing -> Nothing
  Nothing -> Nothing

evalMaybe _ = Nothing
-- Exercise 3
evalMonad :: KULang -> Maybe Int 
evalMonad (Num x) = if x<0 then Nothing else return x

evalMonad (Plus l r) = do {l' <- evalMonad l;
r' <- evalMonad r;
return (l'+r')}

evalMonad (Minus l r) = do {l' <- evalMonad l;
r' <- evalMonad r;
x <- Just (l'-r');
if x<0 then Nothing else return x}

evalMonad (Mult l r) = do {l' <- evalMonad l;
r' <- evalMonad r;
return (l'*r')}

evalMonad (Div l r) = do{
  l' <- evalMonad l; 
  r' <- evalMonad r;
  if r' == 0 then Nothing else return (l' `div` r')
}

evalMonad (Exp l r) = do{
  l' <- evalMonad l; 
  r' <- evalMonad r;
  if r' < 0 then Nothing else return (l' ^ r')
}
evalMonad _ = Nothing

-- Exercise 4
interpret :: String -> Maybe Int 
interpret x = evalMonad (parseKULang x)
interpret _ = Nothing


-- KULang Parser
languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedOpNames = [ "+","-","*","^","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser KULang
expr = buildExpressionParser operators term

operators = [
                [inFix "*" Mult AssocLeft, 
                inFix "/" Div AssocLeft , 
                inFix "+" Plus AssocLeft , 
                inFix "-" Minus AssocLeft, 
                inFix "^" Exp AssocLeft]
            ]
  
numExpr :: Parser KULang 
numExpr = do i <- integer lexer
             return (Num (fromInteger i))
                     

term = parens lexer expr
       <|> numExpr

-- Parser invocation
-- Call parseKULang to parse a string into the KULang data structure.

parseKULang = parseString expr

