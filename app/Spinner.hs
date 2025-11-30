module Spinner (spin, witByName) where

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (find)
import Data.Maybe (fromJust, isNothing)

import qualified Data.Text as T
  ( Text,
    append,
    break,
    cons,
    drop,
    dropWhile,
    find,
    head,
    length,
    pack,
    reverse,
    tail,
    take,
    takeWhile, tails, isPrefixOf, index,
  )

import Spider
import Data.Char (isAlpha)


spin :: String -> IO ([Wit], [Yell])
spin staves = return (ly' (map name . fst) (makeYells (makeWits ("", T.pack staves) [])))

makeYells :: [Wit] -> ([Wit], [Yell])
makeYells wits = (map tellYells wits, yells)
  where
    yells = concatMap reckonYells wits
    tellYells wit@Wit { name } = wit {
      outs = inout fst,
      ins = inout snd
    }
      where inout which = length (filter (== name) (map which yells))
    reckonYells outwit = heads (tails wits)
      where heads = map ((name outwit, ) . name)
            tails = filter isCalled
            isCalled inwit = name inwit /= name outwit && f
              where f = any (\ss -> not (isAlpha (T.index ss (T.length (name inwit))))) (filter (T.isPrefixOf (name inwit)) (T.tails (body outwit)))

makeWits :: (T.Text, T.Text) -> [Wit] -> [Wit]
makeWits (_, "") oldSpine = reverse oldSpine
makeWits (left, right) oldSpine = makeWits (T.cons (T.head right) left, T.tail right) newSpine
  where
    newSpine
      | T.take 4 right == " :: " && isNothing (T.find (==' ') (T.takeWhile (not.isNewline) left)) = w:oldSpine
      | otherwise = oldSpine
    (name, _) = first T.reverse (T.break isNewline left)
    (kind, rest) = T.break isNewline (T.drop 4 right)
    (body, _) = first T.reverse (leap ("", rest))
      where
        leap (l, r)
          | T.length r < 4 = (T.append (T.reverse r) l, "")
          | T.take 4 r == " :: " = (T.dropWhile (not.isNewline) l, r)
          | otherwise = leap (T.cons (T.head r) l, T.tail r)
    w = Wit {
      name,
      kind,
      body,
      ins = -1,
      outs = -1
    }

witByName :: [Wit] -> T.Text -> Wit
witByName wits x = fromJust (find ((== x) . name) wits)

-- gape :: Wit -> Int
-- gape wit = ins wit - outs wit
