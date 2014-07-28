import System.Process
import Control.Monad
import Text.Printf

process :: Int -> (Int,Int) -> IO ()
process indx (sx,sy) = do
  let fn::String
      fn = printf "eval%04d.map" indx
  system $ printf "./mapgen.out %d %d > %s" sx sy fn
  return ()

main::IO ()
main = do
  print geoms
  zipWithM_ process [1..] geoms
  
  
  where
    geoms :: [(Int,Int)]
    geoms = zipWith (\s a -> (s*a,s)) sizes aspectRs
    
    sizes :: [Int]
    sizes = take 100 $ map round sizeRs ++  repeat 256
    sizeRs = [10 * exp(log(256/10) * (fromIntegral  i/79)) | i <- [0..79::Int]]
    
    aspectRs = concat (replicate 10 [1,2]) ++ repeat 1