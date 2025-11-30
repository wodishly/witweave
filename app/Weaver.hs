{-# OPTIONS_GHC -Wno-orphans #-}
{- HLINT ignore "Use infix" -}
module Weaver (weave) where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.Text as T (Text, unpack)

import Diagrams
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude

import Spider
import Spinner
import Data.List (nub)


instance IsName T.Text

weave :: ([Wit], [Yell]) -> IO ()
weave (wits, yells) = (mainWith :: Diagram B -> IO ()) $
  allwork wits yells
  # foldMap (link . bimap (witByName wits) (witByName wits)) yells
  # bgFrame 1 dwhelk
  # lc lwhelk

allwork :: [Wit] -> [Yell] -> Diagram B
allwork wits yells = vsep 1 (map snd (allwork' wits yells []))
-- allwork wits yells = snd (foldr g (99::Int, mempty) (sortBy f (allwork' wits yells [])))
--   where f x y = compare (fst x) (fst y)
--         g (_depth, diagram) (lastDepth, alldiagram) = (lastDepth, alldiagram <> diagram)

allwork' :: [Wit] -> [Yell] -> [T.Text] -> [(Int, Diagram B)]
allwork' [] _ _ = []
allwork' (wit:wits) yells drawns
  | elem (name wit) (map snd yells) = allwork' wits yells drawns
  | otherwise = (depth, diagram) : allwork' wits yells (newDrawns++drawns)
      where (newDrawns, depth, diagram) = allwork'' yells drawns 0 (name wit)

allwork'' :: [(T.Text, T.Text)] -> [T.Text] -> Int  -> T.Text-> ([T.Text], Int, Diagram B)
allwork'' yells drawns depth thisTail = (nub (concat newDrawnses ++ drawns), if null newDepths then depth else maximum newDepths, ) $ knot thisTail
 <> vsep 1 diagrams
  # translateX 6
  where (newDrawnses, newDepths, diagrams) = unzip3 (map (allwork'' badArrows (nub (drawns++newTails)) (succ depth)) newTails)
        (newTails, badArrows) = first (nub . map snd) (sunder f yells)
        f (_, h) = notElem h drawns

knot :: T.Text -> Diagram B
knot name = roundedRect 5 1.5 0.5
  # named name
  <> alignedText 0.5 0.5 (T.unpack name)
  # fc lwhelk

link :: (Wit, Wit) -> Diagram B -> Diagram B
link (startwit, endwit) = (# connectOutside' feather (name startwit) (name endwit))
  where
    feather = with
      & arrowHead .~ thorn
      & headLength .~ small
      & arrowShaft .~ arc xDir (1/6 @@ turn)

lwhelk :: Colour Double
lwhelk = sRGB24read "#ccccff"

dwhelk :: Colour Double
dwhelk = sRGB24read "#363c50"
