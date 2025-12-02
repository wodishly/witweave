module Weaver (weave) where

import Diagrams.TwoD.Combinators (hcat, vcat)

import Spider
import Weever (weeve)


weave :: [Writ] -> IO ()
weave writs = mainWith $ hcat (map (vcat . map weeve) (groupWith howManyIncomes writs))
  where howManyIncomes = length . intersect (map (.nama) writs) . incymas
