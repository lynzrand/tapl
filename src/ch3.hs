data Term
  = TTrue
  | TFalse
  | TIf Term Term Term
  | TZero
  | TSucc Term
  | TPred Term
  | TIsZero Term
  | TStuck
  deriving (Show)

eval :: Term -> Term
eval (TIf TTrue t1 _) = t1
eval (TIf TFalse _ t2) = t2
eval (TIf t t1 t2) = eval $ TIf (eval t) t1 t2
eval (TSucc t) = TSucc (eval t)
eval (TPred TZero) = TZero
eval (TPred (TSucc t)) | isNum t = t
eval (TPred t) = TPred (eval t)
eval (TIsZero TZero) = TTrue
eval (TIsZero (TSucc n)) | isNum n = TFalse
eval (TIsZero t) = TIsZero (eval t)
eval t = error ("No rules apply! " ++ show t)

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
  deriving (Show)

fulleval :: Term -> EvalResult
fulleval t =
  let evt = eval t
   in if isValue evt then Ok t else Stuck t
