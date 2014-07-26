{-# LANGUAGE TemplateHaskell #-}
module SA123N.Types where

import Control.Lens

data Breeder m a = Breeder
  { _mutate :: Double -> a -> m a
  , _crossover :: Double -> a -> a -> m a
  , _triangulate :: a -> a -> a -> m a
  }
  
makeLenses ''Breeder  
