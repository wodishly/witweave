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
    tail,
    tails,
    take,
    takeWhile,
    words, splitAt, unpack,
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
cleanIncomes writs writ@Writ { incomes } = writ { incomes = filter (has (map (.name) writs)) incomes }

makeWits' :: [Wit] -> (T.Text, T.Text) -> [Wit]
makeWits' wits (_, "") = reverse wits
makeWits' wits (forestaves, afterstaves) = makeWits' newWits (leftyoke (forestaves, afterstaves))
  where
    newWits
      | T.splitAt 2 (T.take 4 afterstaves) == (" :", ": ") && isNothing (T.find isSpace (T.takeWhile (not . isNewline) forestaves)) = wit:wits
      | otherwise = wits
    (name, _) = first T.reverse (T.break isNewline forestaves)
    (kind, rest) = T.break isNewline (T.drop 4 afterstaves)
    (body, _) = first T.reverse (leap ("", rest))
      where
        leap (l, r)
          | T.length r < 4 = (T.append (T.reverse r) l, "")
          | T.take 4 r == " :: " = (T.dropWhile (not . isNewline) l, r)
          | otherwise = leap (leftyoke (l, r))
    wit = Wit {
      name,
      kind,
      body,
      inTell = -1,
      outTell = -1
    }

makeAndTellYokes :: [Wit] -> ([Wit], [Yoke])
makeAndTellYokes wits = (map tellYokes wits, yokes)
  where
    yokes = concatMap reckonYokes wits
    tellYokes wit@Wit { name } = wit {
        outTell = inout fst,
        inTell = inout snd
      } where
          inout which = length (filter (== name) (map which yokes))
    reckonYokes Wit { name, body } = headsAnd tails
      where
        headsAnd = map ((name, ) . (.name))
        tails = filter (\x -> all ($ x) [isNotOwnName, isFreestandingName]) wits
        isNotOwnName x = name /= x.name
        isFreestandingName x = has (map trimUnnamies (T.words body)) x.name

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
