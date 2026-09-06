module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]

names :: ASA -> [String]

freshName :: [String] -> String

estaEnLista :: String -> [String] -> Boolean
estaEnLista _ [] = False
estaEnLista x (y:yx) 
    | x == y = True
    | otherwise = estaEnLista x ys

sust :: ASA -> String -> ASA -> ASA
sust (Id x) y exp
    | x == y = exp
    |otherwise = (Id x)
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = (Boolean b)
sust (Not e) x exp = Not (sust e x exp)
sust (Add1 e) x exp = Add1 (sust e x exp)
sust (Sub1 e) x exp = Sub1 (sust e x exp)
sust (ZeroP e) x exp = ZeroP (sust e x exp)
sust (Expt e1 e2) x exp = Expt (sust e1 x exp) (sust e2 x exp)
sust (EqP e1 e2) x exp = EqP (sust e1 x exp) (sust e2 x exp)
    --utilizamos map para aplicar la funcion en todos elemento de cada ASA
sust (And listaASAs) x exp = And (map(\e -> sust e x exp) listaASAs)
sust (Or listaASAs) x exp = Or (map(\e -> sust e x exp) listaASAs)
sust (Add listaASAs) x exp = Add (map(\e -> sust e x exp) listaASAs)
sust (Sub listaASAs) x exp = Sub (map(\e -> sust e x exp) listaASAs)
sust (Mul listaASAs) x exp = Mul (map(\e -> sust e x exp) listaASAs)
sust (Div listaASAs) x exp = Div (map(\e -> sust e x exp) listaASAs)
sust (Lt listaASAs) x exp = Lt (map(\e -> sust e x exp) listaASAs)
sust (Gt listaASAs) x exp = Gt (map(\e -> sust e x exp) listaASAs)
sust (Le listaASAs) x exp = Le (map(\e -> sust e x exp) listaASAs)
sust (Ge listaASAs) x exp = Ge (map(\e -> sust e x exp) listaASAs)

sust (Let bindings e) varObjetivo nuevaExpr =
    let 
        variablesDeclaradas = map (\(x, _) -> x) bindings
        sustitucionBindings = map (\(x, expresiones) -> (x, sust expresiones varObjetivo nuevaExpr)) bindings
        sustitucionCuerpo =
            if(estaEnLista varObjetivo variablesDeclaradas) then e
            else (sust e varObjetivo nuevaExpr)
    in (Let sustitucionBindings sustitucionCuerpo)

sust (LetStar [] e) varObjetivo nuevaExpr = LetStar [] (sust e varObjetivo nuevaExpr)
sust (LetStar ((x,exp): xs) e) varObjetivo nuevaExpr =
    let expr1 = sust expr varObjetivo nuevaExpr
    in if x==varObjetivo then
        LetStar ((x, expr1) : ys) e
        else case sust (LetStar ys e) varObjetivo nuevaExpr of
        LetStar expr1 e1 -> LetStar ((x, expr1): ys) e1
        _ -> error 

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
