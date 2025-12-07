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
import Diagrams.Attributes (lwL, thick)
import Data.Colour.SRGB (Colour, sRGB24read)


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

weeve :: Leaf -> Diagram B
weeve Leaf { name, spells, yokes } =
  wrapWrit writ name
  # named name
  # bgFrame 1 dwhelk
  # lc lwhelk
  where
    writ = layYokesOfSpells spells yokes
      # foldMap (uncurry (on drawArrow (witByName spells))) yokes

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

layYokesOfSpells :: [Spell] -> [Yoke] -> Diagram B
layYokesOfSpells wits yokes = vsep 1 (drawYokesOfSpells wits yokes [])

drawYokesOfSpells :: [Spell] -> [Yoke] -> [T.Text] -> [Diagram B]
drawYokesOfSpells [] _ _ = []
drawYokesOfSpells (wit:wits) yokes drawns
  | has (map snd yokes) wit.name = drawYokesOfSpells wits yokes drawns
  | otherwise = diagram : drawYokesOfSpells wits yokes (newDrawns++drawns)
      where (newDrawns, diagram) = drawSpellAndYokedSpells yokes drawns wit.name

drawSpellAndYokedSpells :: [(T.Text, T.Text)] -> [T.Text] -> T.Text -> ([T.Text], Diagram B)
drawSpellAndYokedSpells yells drawns thisTail = (concat newDrawnses ++ drawns, ) $ drawKnots thisTail
 <> vsep 1 diagrams
  # translateX 9
  where (newTails, badArrows) = first (map snd) (sunder f yells)
        (newDrawnses, diagrams) = unzip (map (drawSpellAndYokedSpells badArrows (drawns++newTails)) newTails)
        f (t, h) = t == thisTail && nas drawns h

drawKnots :: T.Text -> Diagram B
drawKnots name =
  alignedText 0.5 0.5 (T.unpack name)
  # fontSize (local (bestave name))
  # fc lwhelk
 <> roundedRect wide high 0.5
  # named name
  # lwL 0.1
  where V2 wide high = cnottamicel weevewit

bestave :: T.Text -> Double
bestave = min 1 . (staefmicel weevewit /) . fromIntegral . T.length

drawArrow :: Spell -> Spell -> Diagram B -> Diagram B
drawArrow = flip (on ((# connectOutside' feather) # lwL 0.1) (.name))
-- drawArrow = flip (on ((\x y -> (# connectPerim' feather x y halfTurn fullTurn)) # lwL 0.1) (.nama))
  where
  feather = with
    & arrowHead .~ thorn
    & headLength .~ thick

witByName :: [Spell] -> T.Text -> Spell
witByName wits name = fromJust (find ((== name) . (.name)) wits)

lwhelk :: Colour Double
lwhelk = sRGB24read "#ccccff"

dwhelk :: Colour Double
dwhelk = sRGB24read "#363c50"
