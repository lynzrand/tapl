module Tapl.Ch6
  ( UTerm (..),
    NamedEnv,
    createEnv,
    removeNamesImpl,
    removeNames,
    getVarName,
    restoreNamesImpl,
    restoreNamesE,
    restoreNames,
    shiftC,
    shift,
    subst,
    substNamed,
    normalOrderStep,
    traceEval,
    showTrace,
    printTrace,
    eval,
    evalNamed,
    evalNamed',
    module Tapl.Ch5,
  )
where

import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Tapl.Ch5
import Prelude hiding (lookup)

data UTerm
  = UVar Int
  | ULambda UTerm
  | UApply UTerm UTerm
  deriving (Read, Eq)

instance Show UTerm where
  showsPrec _ (UVar i) = shows i
  showsPrec p (ULambda t) =
    showParen (p > prec) $
      showString "\\." . shows t
    where
      prec = 1
  showsPrec p (UApply lhs rhs) =
    showParen (p > prec) $
      showsPrec (prec) lhs . showString " " . showsPrec (prec + 1) rhs
    where
      prec = 2

type NamedEnv = Bimap String Int

createEnv :: [String] -> NamedEnv
createEnv = Bimap.fromList . flip zip [0 ..]

getVarName :: String -> NamedEnv -> Maybe Int
getVarName = Bimap.lookup

-- Remove names

incrDepth :: NamedEnv -> NamedEnv
incrDepth = Bimap.mapR (+ 1)

removeNamesImpl :: NamedEnv -> Term -> Maybe UTerm
removeNamesImpl env (Var v) = getVarName v env >>= Just . UVar
removeNamesImpl env (Lambda v t) =
  let newEnv = incrDepth env
      newEnv' = Bimap.insert v 0 newEnv
   in removeNamesImpl newEnv' t >>= Just . ULambda
removeNamesImpl env (Apply ll rr) = do
  lhs <- removeNamesImpl env ll
  rhs <- removeNamesImpl env rr
  Just $ UApply lhs rhs

removeNames :: Term -> Maybe UTerm
removeNames = removeNamesImpl Bimap.empty

-- Restore names

nextName :: Int -> (String, Int)
nextName m =
  let newName = "_" ++ show m
   in (newName, m + 1)

restoreNamesImpl :: NamedEnv -> Int -> UTerm -> Maybe (Int, Term)
restoreNamesImpl env m (UVar v) = do
  vl <- Bimap.lookupR v env
  Just (m, Var vl)
restoreNamesImpl env m (ULambda t) = do
  let (name, m1) = nextName m
  let newEnv = Bimap.insert name 0 (incrDepth env)
  (m2, body) <- restoreNamesImpl newEnv m1 t
  Just (m2, Lambda name body)
restoreNamesImpl env m (UApply lhs rhs) = do
  (m1, lhs') <- restoreNamesImpl env m lhs
  (m2, rhs') <- restoreNamesImpl env m1 rhs
  Just (m2, Apply lhs' rhs')

restoreNamesE :: NamedEnv -> UTerm -> Maybe Term
restoreNamesE env t = do
  (_, t') <- restoreNamesImpl env 0 t
  Just t'

restoreNames :: UTerm -> Maybe Term
restoreNames t = do
  (_, t') <- restoreNamesImpl Bimap.empty 0 t
  Just t'

-- Substitution

shiftC :: Int -> Int -> UTerm -> UTerm
shiftC cutoff n (UVar i) = if i < cutoff then UVar i else UVar (i + n)
shiftC cutoff n (ULambda t) = ULambda (shiftC (cutoff + 1) n t)
shiftC cutoff n (UApply lhs rhs) = UApply (shiftC cutoff n lhs) (shiftC cutoff n rhs)

shift :: Int -> UTerm -> UTerm
shift = shiftC 0

subst :: Int -> UTerm -> UTerm -> UTerm
subst j s (UVar t) = if j == t then s else UVar t
subst j s (ULambda t) = ULambda (subst (j + 1) (shift 1 s) t)
subst j s (UApply lhs rhs) = UApply (subst j s lhs) (subst j s rhs)

substNamed :: NamedEnv -> String -> Term -> Term -> Maybe Term
substNamed env substitute with using = do
  substId <- getVarName substitute env
  withUTerm <- removeNamesImpl env with
  usingUTerm <- removeNamesImpl env using
  let substResult = subst substId withUTerm usingUTerm
  restoreNamesE env substResult

-- Evaluation

apply :: UTerm -> UTerm -- Must be (UApply (ULambda _) _)
apply (UApply (ULambda t) u) = shift (-1) (subst 0 (shift 1 u) t)
apply _ = error "apply: not a lambda application"

normalOrderStep :: UTerm -> Maybe UTerm
normalOrderStep tt@(UApply (ULambda _) _) = Just $ apply tt
normalOrderStep (UApply t1 t2) =
  let t1' = normalOrderStep t1
   in case t1' of
        Just t1'' -> Just $ UApply t1'' t2
        Nothing -> do
          t2' <- normalOrderStep t2
          Just $ UApply t1 t2'
normalOrderStep (ULambda t) = ULambda <$> normalOrderStep t
normalOrderStep _ = Nothing

type EvalFunc = UTerm -> Maybe UTerm

traceEval :: EvalFunc -> UTerm -> [UTerm]
traceEval f t =
  let impl acc t1 =
        case f t1 of
          Just t' -> impl (t' : acc) t'
          Nothing -> reverse acc
   in impl [t] t

showTrace :: [UTerm] -> String
showTrace = unlines . map show

printTrace :: [UTerm] -> IO ()
printTrace = putStrLn . showTrace

eval :: EvalFunc -> UTerm -> UTerm
eval f t = last $ traceEval f t

evalNamed :: EvalFunc -> Term -> Maybe Term
evalNamed f t = do
  t' <- removeNames t
  restoreNames $ eval f t'

evalNamed' :: NamedEnv -> EvalFunc -> Term -> Maybe Term
evalNamed' env f t = do
  t' <- removeNamesImpl env t
  restoreNamesE env $ eval f t'
