module Main (main) where

import Spinner (spin)
import Weaver (weave)


main :: IO ()
main = weave =<< spin "/Users/clewos/frogwork/app/"
