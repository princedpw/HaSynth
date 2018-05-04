{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveDataTypeable
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

class LRU k t where
  
