module Weaver (weave) where

import Diagrams.Prelude (Diagram)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)

import Diagrams.TwoD.Arrow (connect)
import Diagrams.TwoD.Combinators (hcat, vcat, bgFrame)
import Diagrams.Util ((#))


import Spider
import Weever (weeve)
import Diagrams.Names (IsName (toName))
import Data.Colour.Names (red, blue, green)
import Diagrams.TwoD.Attributes (lc)


weave :: [Writ] -> IO ()
weave writs = mainWith $ yoke writs (hcat (map (vcat . map weeve) (groupWith howManyIncomes writs)))
  where howManyIncomes = length . intersect (map (.name) writs) . incomes

yoke :: [Writ] -> Diagram B -> Diagram B
yoke writs = mconcat (map f writs)
  where
    f Writ { incomes = [] } = mempty
    f Writ { name, incomes } = mconcat (map g incomes)
      where
        g income = connect (ly $ toName name) (ly $ toName income)
          # lc green
