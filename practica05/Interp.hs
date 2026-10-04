module Interp where

import Grammars

data ASA
    = Id Nombre
    | Num Int
    | Boolean Bool
    | Add ASA ASA
    | Sub ASA ASA
    | Not ASA
    | Fun Nombre ASA
    | App ASA ASA
    | If ASA ASA ASA
    deriving (Eq, Show)

data Value
    = NumV Int
    | BooleanV Bool
    | ClosureV Nombre ASA Env
    | ExprV ASA Env
    deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _       = Nothing
curryFun [x] e      = Just (Fun x e) -- como la lambda λx.e
curryFun (x:xs) e
    | elem x xs = Nothing
    | otherwise = Fun x <$> curryFun xs e

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ []       = Nothing
curryApp e (x:xs)   = Just $ foldl App (App e x) xs

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []       = Nothing -- lista vacia
binaryOp _ [x]      = Nothing -- Un solo operando [_]
binaryOp f (x:xs)   = Just $ foldl f x xs 

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] e =  desugar e
desugarCond [(c,r)] e =  desugar(IfS c r e)  --cond((c r) e) = if c r e
desugarCond ((c1, r1): l) e = desugar(IfS c1 r1 (CondS l e))

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS y) = Just (Num y)
desugar (BooleanS z) = Just (Boolean z)
desugar (IfS c t e)=
  let
    Just vc = desugar c
    Just vt = desugar t
    Just ve = desugar e
  in
    Just(If vc vt ve)
--desugar (LetRecS (f s1) s2) = desugar(letS f (App (Ids "y") [FunS [f] s1]) s2)
--Faltan funciones, aplicaciones, Adds, SubS, letS, letStarS, NotS



-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing            
lookupEnv x ((k, v):xs)             
    | x == k    = Just v            
    | otherwise = lookupEnv x xs   

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV x) = Just (NumV x)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x a env) = Just (ClosureV x a env)
strict (ExprV a env)
    |Just e' <- bigStep env a = strict e'
    |otherwise = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
--expresiones atómicas
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
--operaciones aritméticas
bigStep env (Add x y) = 
    let
        Just e1 = bigStep env x
        Just e2 = bigStep env y

        Just (NumV n) = strict e1           
        Just (NumV m) = strict e2
    in Just(NumV (n + m))

bigStep env (Sub x y)= 
    let
        Just e1 = bigStep env x
        Just e2 = bigStep env y

        Just (NumV n) = strict e1           
        Just (NumV m) = strict e2
    in Just(NumV (n + m))
--Fun
bigStep env (Fun p b) = Just (ClosureV p b env)
--App
bigStep env (App f a) =
    let
        Just (ClosureV p b envFun)= bigStep env f
        envNuevo =[(p, (ExprV a env))] ++ envFun
    in bigStep envNuevo b
--If
bigStep env (If a1 a2 a3) =
    let
        Just w = bigStep env a1
        Just(NumV n) = strict w
    in
        if n == 0 then bigStep env a2
        else bigStep env a3
--Not
bigStep env (Not e) =
    let 
        Just w = bigStep env e
        Just (BooleanV b) = strict w
    in Just (BooleanV (not b))
