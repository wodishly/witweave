module Weaver (weave) where

import Diagrams.TwoD.Combinators (hcat, vcat)

import Spider
import Weever (weeve)


weave :: [Leaf] -> IO ()
weave writs = mainWith $ hcat (map (vcat . map weeve) (groupWith howManyIncomes writs))
  where howManyIncomes = length . intersect (map (.name) writs) . incomes
