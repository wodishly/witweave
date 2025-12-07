module Spinner (spin) where

import System.Directory (listDirectory)
import Text.Megaparsec (
  MonadParsec (lookAhead, notFollowedBy, takeWhileP),
  Parsec,
  Stream (Tokens),
  anySingle,
  chunk,
  errorBundlePretty,
  manyTill,
  manyTill_,
  parse,
  skipManyTill,
  )
import Text.Megaparsec.Char (newline, space)

import qualified Data.Text as T (
  Text,
  drop,
  isPrefixOf,
  strip,
  unlines,
  unwords,
  words,
  )
import qualified Data.Text.IO as T (readFile)

import Spider


type Spin = Parsec Void T.Text

spin :: FilePath -> IO [Leaf]
spin road
  | isLeafroad road = shell <$> spinOne road
  | isBranchroad road = spinMany road
  | otherwise = error road

spinMany :: FilePath -> IO [Leaf]
spinMany road = do
  (shortLeafroads, shortOtherRoads) <- sunder isLeafroad <$> listDirectory road
  let shortBranchroads = mapMaybe asBranch shortOtherRoads
      (leafroads, branchroads) = twimap (map (road++)) (shortLeafroads, shortBranchroads)

  underwrits <- concat <$> mapM spin branchroads
  writs <- mapM spinOne leafroads
  return $ cleanIncomes writs++underwrits

asBranch :: FilePath -> Maybe FilePath
asBranch road = if nas road '.'
  then Just (road++"/")
  else Nothing

cleanIncomes :: [Leaf] -> [Leaf]
cleanIncomes writs = map clean writs
  where
    clean writ = writ { incomes = filter isNearby writ.incomes }
    isNearby = has (map (.name) writs)

spinOne :: FilePath -> IO Leaf
spinOne leafroad = bind (T.readFile leafroad) $
  parse spin' "" >>> \case
    Left bundle -> error (errorBundlePretty bundle)
    Right writ -> return writ

spin' :: Spin Leaf
spin' = do
  (_, leafname) <- manyTill_ spinDust spinLeafname
  incomes <- utmostly spinIncome
  spells <- utmostly spinSpell
  return $ Leaf leafname incomes spells (yoke spells)

yoke :: [Spell] -> [Yoke]
yoke wits = concatMap yoke' wits
  where
  yoke' wit =
    filter (isCalledBy wit . snd)
    $ map ((wit.name, ) . (.name)) wits

isCalledBy :: Spell -> Spellname -> Bool
isCalledBy Spell { name, body } = isAllOf [(name /=), has (map shed (T.words body))]

spinDust :: Spin ()
spinDust = do
  line <- lookAhead takeLine
  unless (T.isPrefixOf "module" line) (skipLine >> spinDust)

spinLeafname :: Spin Leafname
spinLeafname = skipWord "module" >> space >> takeWord

spinIncome :: Spin Income
spinIncome = takeLine >>= maybe spinIncome return . asIncome

asIncome :: T.Text -> Maybe Income
asIncome = T.words >>> \case
  "import":"qualified":income:_ -> Just income
  "import":income:_ -> Just income
  _ -> Nothing

spinSpell :: Spin Spell
spinSpell = bind takeLine $
  asSpellmark >>> \case
    Just (name, kind) -> do
      body <- T.strip <$> spinBody
      return $ Spell name kind body 0 0
    Nothing -> spinSpell

asSpellmark :: T.Text -> Maybe (Spellname, Kind)
asSpellmark line = if T.isPrefixOf " " line
  then Nothing
  else case T.words line of
    name:"::":kind -> Just (name, T.unwords kind)
    _ -> Nothing

spinBody :: Spin Body
spinBody = do
  thisBody <- lookAhead takeLine
  if isNothing (asAside thisBody) && isNothing (asSpellmark thisBody)
    then do
      skipLine
      nextBodies <- spinBody
      return $ T.unlines [thisBody, nextBodies]
    else return ""

asAside :: T.Text -> Maybe Aside
asAside line
  | T.isPrefixOf "--" line = Just (T.strip (T.drop 2 line))
  | T.isPrefixOf "{-#" line = Just (T.strip (shave 3 line))
  | T.isPrefixOf "{-" line = Just (T.strip (shave 2 line))
  | otherwise = Nothing

takeWord :: Spin (Tokens T.Text)
takeWord = takeWhileP Nothing (not . isSpace) <* space

takeLine :: Spin T.Text
takeLine = takeWhileP Nothing (not . isNewline) <* newline

skipWord :: T.Text -> Spin ()
skipWord = void . chunk

skipLine :: Spin ()
skipLine = void $ skipAnd newline

skipAnd :: Spin a -> Spin a
skipAnd = skipManyTill anySingle

utmostly :: Spin a -> Spin [a]
utmostly = s (manyTill . skipAnd) notFollowedBy
