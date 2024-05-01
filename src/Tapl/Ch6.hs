module Tapl.Ch6
  ( UTerm (..),
    NamedEnv,
    removeNamesImpl,
    removeNames,
    getVarName,
    restoreNamesImpl,
    restoreNamesE,
    restoreNames,
    shiftC,
    shift,
    subst,
    module Tapl.Ch5,
  )
where

import Data.Bimap
import Data.Bimap qualified as Bimap
import GHC.Show (Show (..), showParen, showString, shows)
import Tapl.Ch5
import Prelude hiding (Show, lookup, show)

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
shiftC cutoff n (ULambda t) = ULambda (shiftC cutoff n t)
shiftC cutoff n (UApply lhs rhs) = UApply (shiftC cutoff n lhs) (shiftC cutoff n rhs)

shift :: Int -> UTerm -> UTerm
shift = shiftC 0

subst :: Int -> UTerm -> UTerm -> UTerm
subst j s (UVar t) = if j == t then s else UVar t
subst j s (ULambda t) = ULambda (subst (j + 1) (shift 1 s) t)
subst j s (UApply lhs rhs) = UApply (subst j s lhs) (subst j s rhs)
