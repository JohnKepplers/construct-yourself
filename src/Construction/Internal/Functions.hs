{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta
  )where

import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute (App p q) x n = App (substitute p x n) (substitute q x n)
substitute l@(Lam y m) x n
  | x == y               = l
  | notMember y (free n) = Lam y (substitute m x n)
  | otherwise            = substitute (alpha l (free n)) x n

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha v@(Var x) s  = v
alpha (App p q) s  = App (alpha p s) (alpha q s)
alpha l@(Lam x m) s 
  | notMember x s = Lam x (alpha m s)
  | otherwise     = Lam freshX (alpha (substitute m x (Var freshX)) s)
     where freshX = fresh x (union s (free l))

-- | beta reduction
beta :: Term -> Term
beta (App (Lam x m) n) = substitute (beta m) x (beta n)
beta (App p q) = App (beta p) (beta q)
beta (Lam x m) = Lam x (beta m)
beta x = x

-- | eta reduction
eta :: Term -> Term
eta l@(Lam x (App m (Var y))) 
  | (notMember x (free m)) && (x == y) = m
  | otherwise                          = l

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'
