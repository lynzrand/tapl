module Tapl.Ch6 (UTerm (..), removeNames, module Tapl.Ch5) where

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

removeNames :: Term -> Maybe UTerm
removeNames t = do
  (term, _) <- removeNamesImpl Data.HashMap.Strict.empty 0 t
  Just term
