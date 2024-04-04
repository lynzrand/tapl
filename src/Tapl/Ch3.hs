module Tapl.Ch3 (Term (..), EvalResult (..), smalleval, bigeval) where

data Term
  = TTrue
  | TFalse
  | TIf Term Term Term
  | TZero
  | TSucc Term
  | TPred Term
  | TIsZero Term
  deriving (Show, Eq)

-- Small-step semantics
small :: Term -> Either String Term
small (TIf TTrue t1 _) = return t1
small (TIf TFalse _ t2) = return t2
small (TIf t t1 t2) = small t >>= \t0 -> return $ TIf t0 t1 t2
small (TSucc t) = return $ TSucc t
small (TPred TZero) = return TZero
small (TPred (TSucc t)) | isNum t = return t
small (TPred t) = small t >>= \t0 -> return $ TPred t0
small (TIsZero TZero) = return TTrue
small (TIsZero (TSucc n)) | isNum n = return TFalse
small (TIsZero t) = small t >>= \t0 -> return $ TIsZero t0
small _ = Left "No rule applies!"

-- Big-step semantics
big :: Term -> Either Term Term
big v | isValue v = return v
big (TIf t1 t2 t3) = do
  et1 <- big t1
  et2 <- big t2
  et3 <- big t3
  case et1 of
    TTrue -> return et2
    TFalse -> return et3
    _ -> Left $ TIf et1 et2 et3
big (TSucc t1) = big t1 >>= \et1 -> return $ TSucc et1
big (TPred t1) = do
  et1 <- big t1
  case et1 of
    (TSucc t) -> return t
    TZero -> return TZero
    _ -> Left $ TPred et1
big (TIsZero t1) = do
  et1 <- big t1
  case et1 of
    (TSucc _) -> return TFalse
    TZero -> return TTrue
    _ -> Left $ TIsZero et1
big t = Left t

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

doeval :: (Term -> Either a Term) -> Term -> EvalResult
doeval ev t =
  case ev t of
    (Left _) -> (if isValue t then Ok t else Stuck t)
    (Right t1) -> doeval ev t1

smalleval :: Term -> EvalResult
smalleval = doeval small

bigeval :: Term -> EvalResult
bigeval t = case big t of
  (Left t1) -> Stuck t1
  (Right t1) -> Ok t1
