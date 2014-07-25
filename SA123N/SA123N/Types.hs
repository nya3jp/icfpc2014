module SA123N.Types where

data Breeder m a =
  { _mutate :: Double -> a -> m a
  , _crossover :: Double -> a -> a -> m a
  , _triangulate :: a -> a -> a -> m a
  }
  
  