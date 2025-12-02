module Main (main) where

import Spinner (spin)
import Weaver (weave)


-- | @cabal run witweave -- -w 800 -h 800 -o witweave.svg@
main :: IO ()
main = weave =<< spin "/Users/clewos/witweave/app/"
