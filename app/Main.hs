module Main (main) where

import Spinner (spin)
import Weaver (weave)

-- |
-- Use @cabal run witweave -- -w 800 -h 800 -o witweave.svg@.
main :: IO ()
main = weave =<< spin =<< readFile "/Users/clewos/frogwork/app/Frog.hs"
