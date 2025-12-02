{- HLINT ignore "Use infix" -}
module Weever (weeve) where

import Diagrams.Prelude (Diagram, local, verySmall, (^/), abbreviatedFields)
import Diagrams.Backend.SVG.CmdLine (B)

import Diagrams.BoundingBox (boundingBox, getCorners)
import Diagrams.Names (IsName, named)
import Diagrams.Points (Point (P))
import Diagrams.TwoD.Arrow (arrowHead, connectOutside', headLength, thorn)
import Diagrams.TwoD.Attributes (fc, lc)
import Diagrams.TwoD.Combinators (bgFrame, vsep)
import Diagrams.TwoD.Shapes (roundedRect)
import Diagrams.TwoD.Text (alignedText, fontSize)
import Diagrams.TwoD.Transform (translate, translateX)
import Diagrams.TwoD.Types (V2 (V2))
import Diagrams.Util (with, (#))

import qualified Data.Text as T (Text, length, unpack)

import Spider
import Diagrams.TwoD.Model (showOrigin, showEnvelope)
import Control.Arrow ((>>>))


instance IsName T.Text

data Settings = Settings {
  writrim :: Double,
  knotgreat :: V2 Double
} deriving (Eq, Show)

witSettings :: Settings
witSettings = Settings {
  writrim = 1,
  knotgreat = V2 5 1.5
}

weeve :: Writ -> Diagram B
weeve Writ { name, wits, yokes } =
  wrapWrit writ name
  # named name
  # bgFrame 1 dwhelk
  # lc lwhelk
  # showOrigin
  where
    b = case getCorners (boundingBox writ) of
      Just (P (V2 x y), P (V2 a b)) -> 1
      Nothing -> error "wah"
    writ = layYokesOfWits wits yokes
      # foldMap (uncurry drawArrows . bimap (witByName wits) (witByName wits)) yokes

wrapWrit :: Diagram B -> T.Text -> Diagram B
wrapWrit diagram writname = vsep 1 [
    --road,-- # rightsteading,
    diagram <> (roundedRect w h 1 # rightsteading)
  ] where
    (P bl, P tr) = fromMaybe (P (V2 0 0), P (V2 0 0)) (getCorners (boundingBox diagram))
    V2 w h = V2 r r + tr - bl
    r = writrim witSettings
    --road = alignedText 0.5 0.5 (T.unpack writname++".hs")
    --  # lc lwhelk
    --  # fc lwhelk
    rightsteading = translate ((V2 (w-5-r) -(h-1.5-r)) ^/ 2)

layYokesOfWits :: [Wit] -> [Yoke] -> Diagram B
layYokesOfWits wits yokes = vsep 1 (drawYokesOfWits wits yokes [])

drawYokesOfWits :: [Wit] -> [Yoke] -> [T.Text] -> [Diagram B]
drawYokesOfWits [] _ _ = []
drawYokesOfWits (wit:wits) yokes drawns
  | elem wit.name (map snd yokes) = drawYokesOfWits wits yokes drawns
  | otherwise = diagram : drawYokesOfWits wits yokes (newDrawns++drawns)
      where (newDrawns, diagram) = drawWitAndYokedWits yokes drawns wit.name

drawWitAndYokedWits :: [(T.Text, T.Text)] -> [T.Text] -> T.Text -> ([T.Text], Diagram B)
drawWitAndYokedWits yells drawns thisTail = (concat newDrawnses ++ drawns, ) $ drawKnots thisTail
 <> vsep 1 diagrams
  # translateX 9
  where (newTails, badArrows) = first (map snd) (sunder f yells)
        (newDrawnses, diagrams) = unzip (map (drawWitAndYokedWits badArrows (drawns++newTails)) newTails)
        f (t, h) = t == thisTail && notElem h drawns

drawKnots :: T.Text -> Diagram B
drawKnots name = alignedText 0.5 0.5 (T.unpack name)
  # fc lwhelk
  # fontSize (local (min 1 (9/fromIntegral (T.length name))))
 <> roundedRect w h 0.5
  # named name
  where V2 w h = knotgreat witSettings

drawArrows :: Wit -> Wit -> Diagram B -> Diagram B
drawArrows endwit startwit = (# connectOutside' feather startwit.name endwit.name)
  where
    feather = with
      & arrowHead .~ thorn
      & headLength .~ verySmall

witByName :: [Wit] -> T.Text -> Wit
witByName wits x = fromJust (find ((== x) . (.name)) wits)
