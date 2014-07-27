#!/usr/bin/env runhaskell


import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf
import Control.Monad

main :: IO ()
main = do
  tmpl <- fmap T.pack $ readFile "middle.sh"
  forM_ [1..100::Int] $ \i -> do
    let con = T.replace (T.pack "eval-2.map") (T.pack $ printf "gen/eval%04d.map" i)  tmpl
        fn = printf "set-a-%04d.sh" i
    T.writeFile fn con
  