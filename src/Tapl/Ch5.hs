module Tapl.Ch5
  ( Term (..),
    l,
    a,
    tid,
    test,
    tru,
    fls,
    tand,
    tor,
    tnot,
    lzero,
    lone,
    churchNum,
  )
where

import Data.String (IsString (..))
import GHC.IsList (IsList (..), Item)

type Var = String

data Term
  = Var Var
  | Lambda Var Term
  | Apply Term Term
  deriving (Show, Eq, Read)

instance IsString Term where
  fromString :: String -> Term
  fromString = Var

instance IsList Term where
  type Item Term = Term
  fromList = a
  toList = error "Not implemented"

-- Shorthand for terms
l :: [Var] -> Term -> Term
l xs t = foldr Lambda t xs

a :: [Term] -> Term
a [] = error "Apply must be called with an argument"
a [t] = t
a (x : xs) = Apply x (a xs)

-- Reductions
doReduce :: Var -> Term -> Term -> Term
doReduce var term (Var v)
  | v == var = term
  | otherwise = Var v
doReduce var term (Apply t1 t2) =
  Apply
    (doReduce var term t1)
    (doReduce var term t2)
doReduce var term (Lambda v t)
  | v == var = Lambda v t -- v is bounded in inner expression
  | otherwise = Lambda v (doReduce var term t)

reduce :: Term -> Term
reduce (Apply (Lambda v e) vv) = doReduce v vv e
reduce _ = error "Not a redex"

-- Identity
tid :: Term
tid = l ["x"] "x"

-- Church bools
tru :: Term
tru = l ["x", "y"] "x"

fls :: Term
fls = l ["x", "y"] "y"

test :: Term
test = tid

-- and = \b \c . b c false
tand :: Term
tand = l ["b", "c"] ["b", "c", fls]

-- or = \b \c . b true c
tor :: Term
tor = l ["b", "c"] ["b", tru, "c"]

-- not = \b . \s \t . b t s
tnot :: Term
tnot = l ["b"] $ l ["s", "t"] ["b", "t", "s"]

-- Church numerals
lzero :: Term
lzero = l ["s", "z"] "z"

lone :: Term
lone = l ["s", "z"] ["s", "z"]

churchNum :: Int -> Term
churchNum n = foldr (\_ x -> Apply "s" x) (Var "z") ([0 .. n] :: [Int])

succ :: Term
succ = l ["n", "s", "z"] ["s", ["n", "s", "z"]]

succ' :: Term
succ' = l ["n", "s", "z"] ["n", "s", ["s", "z"]]
