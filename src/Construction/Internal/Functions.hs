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
substitute (App P Q) x N = App (substitute P x N) (substitute Q x N)
substitute l@(Lam y M) x N
  | x == y               = l
  | notMember y (free N) = Lam y (substitute M x N)
  | otherwise            = substitute (alpha l (free N)) x N

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha v@(Var x) S  = v
alpha (App P Q) S  = App (alpha P S) (alpha Q S)
alpha l@(Lam x M) S 
  | notMember x S = Lam x (alpha M S)
  | otherwise     = Lam freshX (alpha (substitute M x (Var freshX)) S)
     where freshX = fresh x (union S (free l))

-- | beta reduction
beta :: Term -> Term
beta (App (Lam x M) N) = substitute (beta M) x (beta N)
beta (App P Q) = App (beta P) (beta Q)
beta (Lam x M) = Lam x (beta M)
beta x = x

-- | eta reduction
eta :: Term -> Term
eta l@(Lam x (App M x)) 
  | notMember x (free M) = M
  | otherwise            = l
eta x = x

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'
