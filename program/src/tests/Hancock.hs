{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , FunctionalDependencies
           , UndecidableInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}

module Hancock where

import AutotuneTc

class MyEq1 x where
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
  my_eq1 _ _ = False





{- class declarations -}
{- class functionholding -}
class FunctionHolding t

instance FunctionHolding (t funcs) => FunctionHolding (t (funcs,funcs2))

{- enumerable -}
class Enumerable e t where
  elements :: t -> [e]

newtype EnumerableDerived = EnumerableDerived ()

{- insertable -}
class Insertable e t where
  insert :: t -> e -> t

{- emptyable -}
class Emptyable t where
  empty :: t

{- lookupable -}
class Lookupable k v t where
  lookup :: t -> k -> Maybe v

data BST = blah

instance (Eq k, Enumerable (k,v) (t funcs))
  => Lookupable k v (t (EnumerableDerived,funcs)) where
  lookup d k = Prelude.lookup k (elements d)


{- instances -}
{- list -}
newtype List e funcs = List [e]

instance Enumerable e (List e funcs) where
  elements = \(List es) -> es

instance Insertable e (List e funcs) where
  insert (List l) v = List (v:l)

instance Emptyable (List e funcs) where
  empty = List []

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
