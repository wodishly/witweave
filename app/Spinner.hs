module Spinner (spin) where

import qualified Data.Text as T
  ( Text,
    append,
    break,
    cons,
    drop,
    dropWhile,
    find,
    head,
    init,
    isPrefixOf,
    last,
    length,
    null,
    pack,
    reverse,
    splitAt,
    tail,
    tails,
    take,
    takeWhile,
    words,
  )

import Spider


spin :: FilePath -> IO [Writ]
spin staves
  | isHaskell staves = spinOne staves
  | last staves == '/' = spinMany staves
  | otherwise = error "no"

spinOne :: FilePath -> IO [Writ]
spinOne staves = do
  writ <- readFile staves
  let (is, ws) = makeIncomesAndWits (T.pack writ)
      (ws', ys) = makeAndTellYokes ws
  return [Writ (getShortwritname staves) is ws' ys]

spinMany :: FilePath -> IO [Writ]
spinMany staves = do
  allnames <- listDirectory staves
  let shortwritnames = filter isHaskell allnames
      longwritnames = map (staves++) shortwritnames
  writlists <- mapM spinOne longwritnames
  let writlist = concat writlists
  return (map (cleanIncomes writlist) writlist)

makeIncomesAndWits :: T.Text -> ([Income], [Wit])
makeIncomesAndWits writ = (makeIncomes writ, writs)
  where writs = makeWits' [] ("", writ)

makeIncomes :: T.Text -> [Income]
makeIncomes writ = mapMaybe incomeName (T.tails writ)
  where
    incomeName thisTail
      | T.isPrefixOf "\nimport qualified " thisTail = takeName "\nimport qualified " thisTail
      | T.isPrefixOf "\nimport " thisTail = takeName "\nimport " thisTail
      | otherwise = Nothing
    takeName importstaves = Just . T.takeWhile (not . isSpace) . T.drop (T.length importstaves)

cleanIncomes :: [Writ] -> Writ -> Writ
cleanIncomes writs writ@Writ { incymas } = writ { incymas = filter (has (map (.nama) writs)) incymas }

makeWits' :: [Wit] -> (T.Text, T.Text) -> [Wit]
makeWits' wits (_, "") = reverse wits
makeWits' wits (forestaves, afterstaves) = makeWits' newWits (leftyoke (forestaves, afterstaves))
  where
    newWits
      | T.splitAt 2 (T.take 4 afterstaves) == (" :", ": ") && isNothing (T.find isSpace (T.takeWhile (not . isNewline) forestaves)) = wit:wits
      | otherwise = wits
    (nama, _) = first T.reverse (T.break isNewline forestaves)
    (gecynd, rest) = T.break isNewline (T.drop 4 afterstaves)
    (lic, _) = first T.reverse (leap ("", rest))
      where
        leap (l, r)
          | T.length r < 4 = (T.append (T.reverse r) l, "")
          | T.take 4 r == " :: " = (T.dropWhile (not . isNewline) l, r)
          | otherwise = leap (leftyoke (l, r))
    wit = Wit {
      nama,
      gecynd,
      lic,
      ingetael = -1,
      utgetael = -1
    }

makeAndTellYokes :: [Wit] -> ([Wit], [Yoke])
makeAndTellYokes wits = (map tellYokes wits, yokes)
  where
    yokes = concatMap reckonYokes wits
    tellYokes wit@Wit { nama } = wit {
        utgetael = inout fst,
        ingetael = inout snd
      } where
          inout which = length (filter (== nama) (map which yokes))
    reckonYokes Wit { nama, lic } = headsAnd tails
      where
        headsAnd = map ((nama, ) . (.nama))
        tails = filter (\x -> all ($ x) [isNotOwnName, isFreestandingName]) wits
        isNotOwnName x = nama /= x.nama
        isFreestandingName x = has (map trimUnnamies (T.words lic)) x.nama

getShortwritname :: String -> T.Text
getShortwritname = T.reverse . T.takeWhile (/= '/') . T.tail . T.dropWhile (/= '.') . T.reverse . T.pack

isHaskell :: FilePath -> Bool
isHaskell = (".hs" ==) . pord 3

trimUnnamies :: T.Text -> T.Text
trimUnnamies = trimUnnamies' R . trimUnnamies' L

trimUnnamies' :: Hand -> T.Text -> T.Text
trimUnnamies' hand text
  | T.null text = ""
  | isNameworthy (edge text) = text
  | otherwise = trimUnnamies' hand (lave text)
  where
    (edge, lave) = case hand of
      L -> (T.head, T.tail)
      R -> (T.last, T.init)

isNameworthy :: Char -> Bool
isNameworthy = isAnyOf [isAlphaNum, has ['\'', '_']]

isNewline :: Char -> Bool
isNewline = has ['\n', '\r']

leftyoke :: Twain T.Text -> Twain T.Text
leftyoke (ls, rs)
  | T.null rs = (ls, rs)
  | otherwise = (T.cons (T.head rs) ls, T.tail rs)
