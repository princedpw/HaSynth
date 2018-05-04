{-# LANGUAGE TypeFamilies, ScopedTypeVariables, DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , FunctionalDependencies
           , UndecidableInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances #-}

module Dictionary where

import Prelude

class Dictionary k v t | t -> k, t -> v where
  empty  :: t
  insert :: t -> k -> v -> t
  lookup :: t -> k -> Maybe (v,t)

instance Eq k => Dictionary k v [(k,v)] where
  empty        = []
  insert l k v = (k,v):l
  lookup l k   =
    fmap
      (\v -> (v,l))
      (Prelude.lookup k l)
