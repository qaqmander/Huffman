{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Term (module Term) where

data Term t where
    Term :: t -> Integer -> Term t

instance (Eq t) => Eq (Term t) where
    (==) :: Term t -> Term t -> Bool
    (==) (Term v1 t1) (Term v2 t2) = v1 == v2 && t1 == t2

instance (Ord t) => Ord (Term t) where
    (<=) :: Term t -> Term t -> Bool
    (<=) (Term _ t1) (Term _ t2) = t1 <= t2

instance (Show t) => Show (Term t) where
    show :: Term t -> String
    show (Term x val) = "Term " ++ show x ++ " " ++ show val

