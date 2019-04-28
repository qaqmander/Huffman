{-# LANGUAGE InstanceSigs #-}

module Term (
    Term(..),
    combine
) where

data Term t = Term {
    value :: Maybe t,
    tot   :: Integer
}

instance (Eq t) => Eq (Term t) where
    (==) :: Term t -> Term t -> Bool
    (==) (Term _ t1) (Term _ t2) = t1 == t2

instance (Ord t) => Ord (Term t) where
    (<=) :: Term t -> Term t -> Bool
    (<=) (Term _ t1) (Term _ t2) = t1 <= t2

instance (Show t) => Show (Term t) where
    show :: Term t -> String
    show (Term Nothing    tot) = show tot
    show (Term (Just val) tot) = "Term " ++ show val ++ " " ++ show tot

combine :: Term t -> Term t -> Term t
combine (Term _ tot1) (Term _ tot2) = Term Nothing (tot1 + tot2)

