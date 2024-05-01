module Tapl.Ch6
  ( UTerm (..),
    NamedEnv,
    removeNamesImpl,
    removeNamesM,
    removeNames,
    restoreNamesImpl,
    restoreNames,
    module Tapl.Ch5,
  )
where

import Data.HashMap.Strict
import GHC.Show (Show (..))
import Tapl.Ch5
import Prelude hiding (lookup, show)

data UTerm
  = UVar Int
  | ULambda UTerm
  | UApply UTerm UTerm

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

type NamedEnv = HashMap String Int

getVar :: String -> NamedEnv -> Maybe Int
getVar = lookup

-- Remove names

removeNamesImpl :: NamedEnv -> Int -> Term -> Maybe (UTerm, Int)
removeNamesImpl env maxid (Var name) = do
  varid <- getVar name env
  Just (UVar varid, maxid)
removeNamesImpl env maxid (Lambda var term) =
  let newMax = maxid + 1
      varName = maxid
      newEnv = insert var varName env
   in do
        (newTerm, finalMax) <- removeNamesImpl newEnv newMax term
        Just (ULambda newTerm, finalMax)
removeNamesImpl env maxid (Apply lhs rhs) =
  do
    (lhs1, max1) <- removeNamesImpl env maxid lhs
    (rhs1, max2) <- removeNamesImpl env max1 rhs
    Just (UApply lhs1 rhs1, max2)

removeNamesM :: Int -> Term -> Maybe UTerm
removeNamesM m t = do
  (term, _) <- removeNamesImpl Data.HashMap.Strict.empty m t
  Just term

removeNames :: Term -> Maybe UTerm
removeNames = removeNamesM 0

-- Restore names

data RestoreEnv = RestoreEnv
  { maxId :: Int,
    nameEnv :: HashMap Int String
  }

emptyRestore :: RestoreEnv
emptyRestore = RestoreEnv {maxId = 0, nameEnv = empty}

nextName :: RestoreEnv -> (String, RestoreEnv)
nextName env =
  let newId = env.maxId
      newName = "_" ++ show newId
      newEnv = insert newId newName env.nameEnv
   in (newName, RestoreEnv {maxId = newId + 1, nameEnv = newEnv})

restoreNamesImpl :: RestoreEnv -> UTerm -> Maybe (RestoreEnv, Term)
restoreNamesImpl env (UVar v) = do
  vl <- lookup v env.nameEnv
  Just (env, Var vl)
restoreNamesImpl env (ULambda t) =
  let (name, env1) = nextName env
   in do
        (env2, rhs) <- restoreNamesImpl env1 t
        Just (env2, Lambda name rhs)
restoreNamesImpl env (UApply lhs rhs) = do
  (env1, lhs1) <- restoreNamesImpl env lhs
  (env2, rhs1) <- restoreNamesImpl env1 rhs
  Just (env2, Apply lhs1 rhs1)

restoreNames :: UTerm -> Maybe Term
restoreNames term = do
  (_, res) <- restoreNamesImpl emptyRestore term
  Just res
