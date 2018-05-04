{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , DataKinds
           , FunctionalDependencies
           , AllowAmbiguousTypes
           , TypeApplications
           , TypeOperators
           , GADTs
           , UndecidableInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}

module Main where

import AutotuneTc
import Test.Hspec
import Prelude
import NatHolding
import Data.Singletons.TH

my_function :: (NatHolding h, h ~ j) => SNat j -> SNat j
my_function x = x

class LeakyDict k v t | t -> k, t -> v where
  leaky_empty :: t
  leaky_insert :: t -> k -> v -> t
  leaky_find :: t -> k -> (Maybe v,t)

class Dict k v t | t -> k, t -> v  where
  empty :: t
  insert :: t -> k -> v -> t
  find :: t -> k -> (Maybe v,t)

instance (Dict k v t) => LeakyDict k v t where
  leaky_empty = empty
  leaky_insert = insert
  leaky_find = find

instance Eq k => Dict k v [(k,v)] where
  empty = []
  insert d k v = (k,v):d
  find d k = (lookup k d,d)

instance (Eq k, NatHolding n,Even n ~ 'True)
      => LeakyDict k v ([(k,v)],Nat) where
  leaky_empty = ([],Zero)
  leaky_insert (d,i) k v =
    if (val @n == i) then
      ((k,v):(init d),i)
    else
      ((k,v):d,Succ(i))
  leaky_find (d,i) k =
    (lookup k d,(d,i))

instance (LeakyDict k v ld, Dict k v d)
      => Dict k v (ld,d) where
  empty = (leaky_empty,empty)
  insert (ld,d) k v = (leaky_insert ld k v,insert d k v)
  find (ld,d) k =
    case leaky_find ld k of
      (Just v ,ld) -> (Just v,(ld,d))
      (Nothing,ld) ->
        case find d k of
          (Just v ,d) -> (Just v,(leaky_insert ld k v,d))
          (Nothing,d) -> (Nothing,(ld,d))

data CurriedDict d = CurriedDict d

instance (Dict k1 d2 d1, Dict k2 v2 d2)
      => Dict (k1,k2) v2 (CurriedDict d1) where
  empty = empty
  insert (CurriedDict d1) (k1,k2) v =
    case find d1 k1 of
      (Just d2,d1) ->
        let d2 = insert d2 k2 v in
        CurriedDict (insert d1 k1 d2)
      (Nothing,d1) ->
        let d2 = insert empty k2 v in
        CurriedDict (insert d1 k1 d2)
  find (CurriedDict d1) (k1,k2) =
    case find d1 k1 of
      (Just d2,d1) ->
        let (m_v,d2) = find d2 k2 in
        let d1 = insert d1 k1 d2 in
        (m_v,CurriedDict d1)
      (Nothing,d1) ->
        (Nothing,CurriedDict d1)

{-data S x = S x
data D x = D x

type family Even (n::Nat) :: Bool
type instance Even Zero = True
type instance Even (Succ n) = Not (Even n)

data Z = Z
instance NatHolding Z where
  type NatType Z = Zero

instance NatHolding x => NatHolding (S x) where
  type NatType (S x) = Succ (NatType x)

instance NatHolding x => NatHolding (D x) where
  type NatType (D x) = Add (NatType x) (NatType x)-}


{-class MyEq1 x where
  my_eq1 :: x -> x -> Bool

class MyComp x where
  comp :: x -> x -> Int

instance MyComp x => MyEq1 x where
  my_eq1 = \x y -> comp x y == 0

data MyTypeCompare = DataLeft | DataRight

instance MyComp MyTypeCompare where
  comp DataLeft DataRight = -1
  comp DataRight DataLeft = 1
  comp _ _ = 0

instance MyEq1 MyTypeCompare where
  my_eq1 DataLeft DataLeft = False
  my_eq1 DataRight DataRight = True
  my_eq1 _ _ = False-}





{- class declarations -}
{- class functionholding -}
{-class FunctionHolding t

instance FunctionHolding (t funcs) => FunctionHolding (t (funcs,funcs2))-}

{- enumerable -}
{-class Enumerable e t where
  elements :: t -> [e]

newtype EnumerableDerived = EnumerableDerived ()-}

{- insertable -}
{-class Insertable e t where
  insert :: t -> e -> t-}

{- emptyable -}
{-class Emptyable t where
  empty :: t-}

{- lookupable -}
{-class Lookupable k v t where
  lookup :: t -> k -> Maybe v

data BST = blah

instance (Eq k, Enumerable (k,v) (t funcs))
  => Lookupable k v (t (EnumerableDerived,funcs)) where
  lookup d k = Prelude.lookup k (elements d)-}


{- instances -}
{- list -}
{-newtype List e funcs = List [e]

instance Enumerable e (List e funcs) where
  elements = \(List es) -> es

instance Insertable e (List e funcs) where
  insert (List l) v = List (v:l)

instance Emptyable (List e funcs) where
  empty = List []-}

{-instance Enumerable e t DerivedInstance
      => Dictionary e t DerivedInstance
  where
    myfilter = \f s -> filter f (elements s)-}



{-[autotune_tc|
class Dict k v t where
  empty  :: t k v
  insert :: t k v -> k -> v -> t k v
  lookup :: t k v -> k -> Maybe v

newtype AssocList k v functions = AssocList [(k,v)]
newtype HashDict k v = HashDict [(k,v)]

instance Eq k => Dict k v AssocList where
  empty = AssocList []
  insert = \(AssocList l) k v ->
    AssocList ((k,v):l)
  lookup = \(AssocList l) k -> Prelude.lookup k l

class MyEq a where
  my_eq :: a -> a -> Bool

class  (MyEq a) => MyOrd a  where
  (<), (<=), (>=), (>)  :: a -> a -> Bool
  max, min              :: a -> a -> a
|]-}

main :: IO ()
main = hspec $ do
  describe "QuasiQuoter" $ do
    describe "Validate haqify function" $ do
      it "haqify is supposed to prefix Haq! to things1" $ do
        "hi" `shouldBe` "hi"
    describe "Validate haqify' function" $ do
      it "haqify is supposed to prefix Haq! to things2" $ do
        "hi" `shouldBe` "hi"
