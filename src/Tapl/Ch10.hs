module Tapl.Ch10
  ( Ty (..),
    Term (..),
    Pat (..),
    (->>),
    isValue,
    typeof,
  )
where

import Control.Monad

data Ty
  = TyBool
  | TyA
  | TyUnit
  | TyArrow Ty Ty
  | TyTuple [Ty]
  | TyRecord [(String, Ty)]
  deriving (Eq, Read, Show)

data Pat
  = PVar String
  | PRecord [(String, Pat)]
  deriving (Eq, Read, Show)

data Term
  = Bool Bool
  | Lambda String Ty Term
  | Var String
  | Apply Term Term
  | If Term Term Term
  | Seq Term Term
  | Unit
  | As Term Ty
  | Let Pat Term Term
  | Tuple [Term]
  | TupleProj Term Int
  | Record [(String, Term)]
  | RecordProj Term String
  deriving (Eq, Read, Show)

-- Values

isValue :: Term -> Bool
isValue Unit = True
isValue (Bool _) = True
isValue (Lambda {}) = True
isValue _ = False

-- Typing

infixr 5 ->>

(->>) :: Ty -> Ty -> Ty
(->>) = TyArrow

type TypeEnv = [(String, Ty)]

-- t2 can be assigned to a term expecting t1
assignableAs :: Ty -> Ty -> Bool
assignableAs t1 t2 = t1 == t2 -- This is rather uninteresting for now

upperBound :: Ty -> Ty -> Maybe Ty
upperBound t1 t2 = if t1 == t2 then return t1 else Nothing -- And this too

match :: TypeEnv -> Pat -> Ty -> Maybe TypeEnv
match env (PVar x) ty = return ((x, ty) : env)
match env (PRecord ps) (TyRecord ts) = do
  foldM
    ( \env' (l, p) -> do
        ty <- lookup l ts
        match env' p ty
    )
    env
    ps
match _ _ _ = Nothing

typeof :: TypeEnv -> Term -> Maybe Ty
typeof _ Unit = return TyUnit
typeof _env (Bool _) = return TyBool
typeof env (Lambda x ty term) = do
  let newEnv = (x, ty) : env
  ty' <- typeof newEnv term
  return (ty ->> ty')
typeof env (Var v) = lookup v env
typeof env (Apply lhs rhs) = do
  TyArrow ty1 ty2 <- typeof env lhs
  ty1' <- typeof env rhs
  if assignableAs ty1 ty1'
    then return ty2
    else Nothing
typeof env (If cond t f) = do
  TyBool <- typeof env cond
  ty1 <- typeof env t
  ty2 <- typeof env f
  upperBound ty1 ty2
typeof env (Seq t1 t2) = do
  TyUnit <- typeof env t1
  typeof env t2
typeof env (As term ty) = do
  ty' <- typeof env term
  if assignableAs ty ty'
    then return ty
    else Nothing
typeof env (Let x t1 t2) = do
  ty1 <- typeof env t1
  newEnv <- match env x ty1
  typeof newEnv t2
-- Thanks Copilot for generating these type checkers
typeof env (Tuple ts) = TyTuple <$> mapM (typeof env) ts
typeof env (TupleProj t i) = do
  TyTuple ts <- typeof env t
  if i < length ts
    then return (ts !! i)
    else Nothing
typeof env (Record fields) = TyRecord <$> mapM (\(l, t) -> (,) l <$> typeof env t) fields
typeof env (RecordProj t l) = do
  TyRecord fields <- typeof env t
  lookup l fields
