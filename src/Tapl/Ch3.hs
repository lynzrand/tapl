module Tapl.Ch3 where

data Term
  = TTrue
  | TFalse
  | TIf Term Term Term
  | TZero
  | TSucc Term
  | TPred Term
  | TIsZero Term
  deriving (Show, Eq)

eval :: Term -> Either String Term
eval (TIf TTrue t1 _) = Right t1
eval (TIf TFalse _ t2) = Right t2
eval (TIf t t1 t2) = eval t >>= \t0 -> return $ TIf t0 t1 t2
eval (TSucc t) = Right $ TSucc t
eval (TPred TZero) = Right TZero
eval (TPred (TSucc t)) | isNum t = Right t
eval (TPred t) = eval t >>= \t0 -> return $ TPred t0
eval (TIsZero TZero) = Right TTrue
eval (TIsZero (TSucc n)) | isNum n = Right TFalse
eval (TIsZero t) = eval t >>= \t0 -> return $ TIsZero t0
eval _ = Left "No rules applies!"

isNum :: Term -> Bool
isNum TZero = True
isNum (TSucc v) = isNum v
isNum _ = False

isBool :: Term -> Bool
isBool TTrue = True
isBool TFalse = True
isBool _ = False

isValue :: Term -> Bool
isValue t = isNum t || isBool t

data EvalResult = Ok Term | Stuck Term
  deriving (Show, Eq)

fulleval :: Term -> EvalResult
fulleval t =
  let evt = eval t
   in case evt of
        (Left _) -> (if isValue t then Ok t else Stuck t)
        (Right t1) -> fulleval t1
