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

eval :: Term -> Either Term Term
eval (TIf TTrue t1 _) = Right t1
eval (TIf TFalse _ t2) = Right t2
eval (TIf t t1 t2) =
  let evt = eval t
   in case evt of
        (Right t0) -> Right $ TIf t0 t1 t2
        (Left _) -> Left t
eval (TSucc t) = Right $ TSucc t
eval (TPred TZero) = Right TZero
eval (TPred (TSucc t)) | isNum t = Right t
eval (TPred t) =
  let evt = eval t
   in case evt of
        (Right t0) -> Right $ TPred t0
        (Left _) -> Left t
eval (TIsZero TZero) = Right TTrue
eval (TIsZero (TSucc n)) | isNum n = Right TFalse
eval (TIsZero t) =
  let evt = eval t
   in case evt of
        (Right t0) -> Right $ TIsZero t0
        (Left _) -> Left t
eval t = Left t

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
   in case evt of
        (Left t1) -> (if isValue t1 then Ok t1 else Stuck t1)
        (Right t1) -> fulleval t1
