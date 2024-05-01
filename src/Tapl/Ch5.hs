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
    scc,
    scc',
    churchNum,
    (\\),
  )
where

import Data.String (IsString (..))
import GHC.IsList (IsList (..), Item)
import GHC.Show (Show (..), showParen, showString, shows)
import Prelude hiding (Show, show)

type Var = String

data Term
  = Var Var
  | Lambda Var Term
  | Apply Term Term
  deriving (Eq, Read)

instance Show Term where
  showsPrec _ (Var i) = showString i
  showsPrec p (Lambda v t) =
    showParen (p > prec) $
      showString "\\" . showString v . showString ". " . shows t
    where
      prec = 1
  showsPrec p (Apply lhs rhs) =
    showParen (p > prec) $
      showsPrec prec lhs . showString " " . showsPrec (prec + 1) rhs
    where
      prec = 2

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

(\\) :: [Var] -> Term -> Term
(\\) = l

a :: [Term] -> Term
a xs =
  let aImpl [] = error "Apply must be called with an argument"
      aImpl [x] = x
      aImpl (x : xss) = Apply (aImpl xss) x
   in aImpl (reverse xs)

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
churchNum n = l ["s", "z"] (foldr (\_ x -> Apply "s" x) (Var "z") ([0 .. (n - 1)] :: [Int]))

scc :: Term
scc = l ["n", "s", "z"] ["s", ["n", "s", "z"]]

scc' :: Term
scc' = l ["n", "s", "z"] ["n", "s", ["s", "z"]]
