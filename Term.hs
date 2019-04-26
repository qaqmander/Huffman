{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Term(module Term) where

data Term t where
    Term :: (Show t, Eq t) => t -> Integer -> Term t

instance Eq (Term t) where
    (==) :: Term t -> Term t -> Bool
    (==) (Term v1 t1) (Term v2 t2) = v1 == v2 && t1 == t2

instance Ord (Term t) where
    (<=) :: Term t -> Term t -> Bool
    (<=) (Term _ t1) (Term _ t2) = t1 <= t2

