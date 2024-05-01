module Tapl.Ch10
  ( Ty (..),
    Term (..),
    (->>),
    getType,
  )
where

data Ty
  = TyBool
  | TyA
  | TyUnit
  | TyArrow Ty Ty
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
  deriving (Eq, Read, Show)

infixr 5 ->>

(->>) :: Ty -> Ty -> Ty
(->>) = TyArrow

type TypeEnv = [(String, Ty)]

-- t2 can be assigned to a term expecting t1
assignableAs :: Ty -> Ty -> Bool
assignableAs t1 t2 = t1 == t2 -- This is rather uninteresting for now

upperBound :: Ty -> Ty -> Maybe Ty
upperBound t1 t2 = if t1 == t2 then return t1 else Nothing

getType :: TypeEnv -> Term -> Maybe Ty
getType _ Unit = return TyUnit
getType _env (Bool _) = return TyBool
getType env (Lambda x ty term) = do
  let newEnv = (x, ty) : env
  ty' <- getType newEnv term
  return (ty ->> ty')
getType env (Var v) = lookup v env
getType env (Apply lhs rhs) = do
  TyArrow ty1 ty2 <- getType env lhs
  ty1' <- getType env rhs
  if assignableAs ty1 ty1'
    then return ty2
    else Nothing
getType env (If cond t f) = do
  TyBool <- getType env cond
  ty1 <- getType env t
  ty2 <- getType env f
  upperBound ty1 ty2
getType env (Seq t1 t2) = do
  TyUnit <- getType env t1
  getType env t2
getType env (As term ty) = do
  ty' <- getType env term
  if assignableAs ty ty'
    then return ty
    else Nothing
