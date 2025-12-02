module Weever (weeve) where

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


instance IsName T.Text

data Weevewit = Weevewit {
  gewritrima :: Double,
  cnottamicel :: V2 Double,
  staefmicel :: Double
} deriving (Eq, Show)

weevewit :: Weevewit
weevewit = Weevewit {
  gewritrima = 1,
  cnottamicel = V2 5 1.5,
  staefmicel = 9
}

weeve :: Writ -> Diagram B
weeve Writ { nama, wittu, geocu } =
  wrapWrit writ nama
  # named nama
  # bgFrame 1 dwhelk
  # lc lwhelk
  where
    writ = layYokesOfWits wittu geocu
      # foldMap (uncurry (on drawArrow (witByName wittu))) geocu

wrapWrit :: Diagram B -> T.Text -> Diagram B
wrapWrit diagram writname = vsep 1 [
    road # rightsteading,
    diagram <> (roundedRect wide high 1 # rightsteading)
  ] where
    (P bole, P tori) = fromMaybe (P (V2 0 0), P (V2 0 0)) (getCorners (boundingBox diagram))
    V2 wide high = V2 rim rim + tori - bole
    rim = gewritrima weevewit
    road = alignedText 0.5 0.5 (T.unpack writname++".hs")
      # lc lwhelk
      # fc lwhelk
    rightsteading = translate ((V2 (wide-5-rim) -(high-1.5-rim)) ^/ 2)

layYokesOfWits :: [Wit] -> [Yoke] -> Diagram B
layYokesOfWits wits yokes = vsep 1 (drawYokesOfWits wits yokes [])

drawYokesOfWits :: [Wit] -> [Yoke] -> [T.Text] -> [Diagram B]
drawYokesOfWits [] _ _ = []
drawYokesOfWits (wit:wits) yokes drawns
  | has (map snd yokes) wit.nama = drawYokesOfWits wits yokes drawns
  | otherwise = diagram : drawYokesOfWits wits yokes (newDrawns++drawns)
      where (newDrawns, diagram) = drawWitAndYokedWits yokes drawns wit.nama

drawWitAndYokedWits :: [(T.Text, T.Text)] -> [T.Text] -> T.Text -> ([T.Text], Diagram B)
drawWitAndYokedWits yells drawns thisTail = (concat newDrawnses ++ drawns, ) $ drawKnots thisTail
 <> vsep 1 diagrams
  # translateX 9
  where (newTails, badArrows) = first (map snd) (sunder f yells)
        (newDrawnses, diagrams) = unzip (map (drawWitAndYokedWits badArrows (drawns++newTails)) newTails)
        f (t, h) = t == thisTail && nas drawns h

drawKnots :: T.Text -> Diagram B
drawKnots name =
  alignedText 0.5 0.5 (T.unpack name)
  # fontSize (local (bestave name))
  # fc lwhelk
 <> roundedRect wide high 0.5
  # named name
  where V2 wide high = cnottamicel weevewit

bestave :: T.Text -> Double
bestave = min 1 . (staefmicel weevewit /) . fromIntegral . T.length

drawArrow :: Wit -> Wit -> Diagram B -> Diagram B
drawArrow = flip (on (# connectOutside' feather) (.nama))
  where
  feather = with
    & arrowHead .~ thorn
    & headLength .~ verySmall

witByName :: [Wit] -> T.Text -> Wit
witByName wits name = fromJust (find ((== name) . (.nama)) wits)
