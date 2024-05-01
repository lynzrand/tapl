module Tapl.Ch8
  ( Ty (..),
    Term (..),
    (->>),
    getType,
  )
where

data Ty
  = TyBool
  | TyArrow Ty Ty
  deriving (Eq, Read, Show)

data Term
  = TBool Bool
  | TLambda String Ty Term
  | TVar String
  | TApply Term Term
  | TIf Term Term Term
  deriving (Eq, Read, Show)

infixr 5 ->>

(->>) :: Ty -> Ty -> Ty
(->>) = TyArrow

type TypeEnv = [(String, Ty)]

getType :: TypeEnv -> Term -> Maybe Ty
getType _env (TBool _) = return TyBool
getType env (TLambda x ty term) = do
  let newEnv = (x, ty) : env
  ty' <- getType newEnv term
  return (ty ->> ty')
getType env (TVar v) = lookup v env
getType env (TApply lhs rhs) = do
  TyArrow ty1 ty2 <- getType env lhs
  ty1' <- getType env rhs
  if ty1 == ty1'
    then return ty2
    else Nothing
getType env (TIf cond t f) = do
  TyBool <- getType env cond
  ty1 <- getType env t
  ty2 <- getType env f
  if ty1 == ty2
    then return ty1
    else Nothing
