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
    deriving (Eq, Show)

data Value
    = NumV Int
    | BooleanV Bool
    | ClosureV Nombre ASA Env
    deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _       = Nothing
curryFun [x] e      = Just (Fun x e) -- como la lambda λx.e
curryFun (x:xs) e
    | elem x xs = Nothing -- revisar
    | otherwise = Fun x <$> curryFun xs e
-- tal vez sea mas haskell idiomatica utilizar applicative como en la practica anterior "Amy" y Estefy

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ []       = Nothing
curryApp e (x:xs)   = Just $ foldl App (App e x) xs -- creo debe ser de izquierda a derechar para separar las aplicaciones como en matematicas con la notación prefija
-- creo que esto debería de heredar la instancia de clase Maybe Foldable ASA => Asa -> [ASA] -> Maybe ASA "Amy"

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []       = Nothing -- lista vacia
binaryOp _ [x]      = Nothing -- Un solo operando [_]
binaryOp f (x:xs)   = Just $ foldl f x xs -- por hipótesis de que f sea una aplicación binaria, x y xs una expresión; f $ (f (f a b) c) d
-- creo esto forma un monoide


-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
-- rola para esta sección: https://tidal.com/track/61136961/u
-- El SASA está en el Grammars.y
-- Expresiones atómicas
desugar (IdS x)                 = Just $ Id x
desugar (NumS y)                = Just $ Num y
desugar (BooleanS z)            = Just $ Boolean z

-- Funiciones
desugar (FunS e cuerpo)         = do
    cuerpoA <- desugar cuerpo
    curryFun e cuerpoA
-- Supongamos que e es una funcion currificada, primero desazucaramos el cuerpo para obtener la lista que se pera de curryFun y aplicamos el ASA

-- aplicaciones con curry
desugar (AppS s p0)             = do
    t <- desugar s
    p1 <- mapM desugar p0
    -- ALgo similar a lo de arriba
    curryApp t p1

-- las de las binOp (creo se lee mejor que binaryOp), y supongo que podria abstraerse a algo más general
desugar (AddS p)                = do
    p1 <- mapM desugar p
    binaryOp Add p1

desugar (SubS p)                = do
    p1 <- mapM desugar p
    binaryOp Sub p1 -- extraño el binOp

-- La aplicación de mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m () se usa para aplicar en forma de functor o aplicative hacia listas en lugar de usar <$> en un applicative, dependiendo del prelude este puede ser o no de tipo Mafbe Monad (que por transitividad un functor es applicative y applicative es un paso menos abstracto para una monad) ref: https://hoogle.haskell.org/?q=mapM_

-- let normal (nuestro Let)
desugar (LetS lambda x y)       = do
    a <- desugar x
    b <- desugar y
    Just $ App (Fun lambda b) a
    -- Esto requiere Justo ya que con las funciones anteriores ya es de tipo Mayxbe, a excepcion de esto

-- let shiny
desugar (LetStarS [] cuerpo)                    = desugar cuerpo -- me parece que esto solo regresa la variable que halla
desugar (LetStarS ((x, s):bindings) cuerpo)     = desugar (LetS x s (LetStarS bindings cuerpo)) -- se hace recursión sobre una lista con elemento(s) y se pasa la lógica de LetS hacia todo su cuerpo, creo esto tambien debería ser applicative en su defecto monádico con mapM
-- a este si casi no le entendí :'v

-- nuestr NOT
desugar (NotS e)                = do
    prop <- desugar e
    Just $ Not prop

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing            -- no es como que haya algún lugar en donde buscar
lookupEnv x ((k, v):xs)             -- recursión sobre listo de tipo tupla
    | x == k    = Just v            -- aunque se puede hacer snd, pero como se especifica como caso recursivo no se puede usar
    | otherwise = lookupEnv x xs    -- busca en el siguiente

-- Nota, en haskell ya existe lookup, creo esto se podria aprovechar usando mejor el metalenguaje
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b; ref: https://hoogle.haskell.org/?hoogle=lookup
-- además de venir con point-free, lo que lo hace mas haskell idiomatico
-- lookupEnv = lookup

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
-- esto tiene una aplicacion de applicative y/o monad
-- expr atomicas
bigStep entorno (Id x)          = lookupEnv x entorno
bigStep _ (Num y)               = Just $ NumV y
bigStep _ (Boolean z)           = Just $ BooleanV z
-- no se si es Num valor o Num Value

-- operaciones aritmeticas
bigStep entorno (Add x y)       = do
    n1 <- bigStep entorno x
    n2 <- bigStep entorno y
    case (n1, n2) of
        (NumV n1, NumV n2)  -> Just $ NumV (n1 + n2)
        _                   -> Nothing

bigStep entorno (Sub x y)       = do
    n1 <- bigStep entorno x
    n2 <- bigStep entorno y
    case (n1, n2) of
        (NumV n1, NumV n2)  -> Just $ NumV (max 0 (n1 - n2))
        _                   -> Nothing

-- función estable/cerrada
bigStep entorno (Fun x cuerpo)  = Just $ ClosureV x cuerpo entorno

-- aplicación de aplicaciones
bigStep entorno (App x y)       = do
    vFuncion <- bigStep entorno x
    vParam <- bigStep entorno y
    case vFuncion of
        ClosureV z cuerpo entornoDef    -> bigStep ((z, vParam):entornoDef) cuerpo -- recursión sobre las tuplas en las que se hizo el paso grande
        _                               -> Nothing -- si no esta la estructura estable en particular

-- nuestro Not
bigStep entorno (Not e) = do
    valor <- bigStep entorno e
    case valor of
        BooleanV a  -> Just $ BooleanV (not a)
        _           -> Nothing

-- si se usa applicative, el desultado de las evaluaciones indica en que entorno se usará bajo la aplicacios, similar a un functor
-- el caso _ simplemente no se evalua y roterna nothing evitando que todo exlpote

