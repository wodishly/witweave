module Spider (
  module Spider,
  module Control.Arrow,
  module Control.Lens,
  module Control.Monad,
  module Data.Bifunctor,
  module Data.Char,
  module Data.Foldable,
  module Data.Function,
  module Data.Functor,
  module Data.List,
  module Data.Maybe,
  module Data.Void,
  module GHC.Exts,
  module Diagrams.Prelude,
  module Diagrams.Backend.SVG.CmdLine,
) where

import Control.Arrow ((>>>))
import Control.Lens ((.~), (^.))
import Control.Monad (unless, void)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (find)
import Data.Function (on, (&))
import Data.Functor ((<&>))
import Data.List (intersect)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Void (Void)
import Debug.Trace (trace)
import GHC.Exts (groupWith)

import Diagrams.Prelude (Diagram, local, verySmall, (^/))
import Diagrams.Backend.SVG.CmdLine (B, mainWith)

import qualified Data.Text as T (Text, drop, head, reverse, tail, unpack)


data Hand = L | R deriving (Eq, Show)
type Twain a = (a, a)
type Aside = T.Text

type Leafname = T.Text
type Income = T.Text
type Yoke = Twain T.Text

data Leaf = Leaf {
  name :: Leafname,
  incomes :: [Income],
  spells :: [Spell],
  yokes :: [Yoke]
} deriving (Eq, Show)

type Spellname = T.Text
type Kind = T.Text
type Body = T.Text

data Spell = Spell {
  name :: Spellname,
  kind :: Kind,
  body :: Body,
  outs :: Int,
  ins :: Int
} deriving Eq

instance Show Spell where
  show Spell { name, kind, body } =
       T.unpack name ++ " :: "
    ++ T.unpack kind ++ "\n"
    ++ T.unpack body

{-# INLINE ly #-}
ly :: Show a => a -> a
ly = ly' id

{-# INLINE ly' #-}
ly' :: Show a => (b -> a) -> b -> b
ly' f x = trace (show (f x)) x

{-# INLINE s #-}
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

{-# INLINE w #-}
w :: (a -> a -> b) -> a -> b
w f x = f x x

{-# INLINE shell #-}
shell :: a -> [a]
shell = (:[])

{-# INLINE full #-}
full :: Foldable t => t a -> Bool
full = not . null

{-# INLINE has #-}
has :: (Foldable t, Eq a) => t a -> a -> Bool
xs `has` x = x `elem` xs

{-# INLINE nas #-}
nas :: (Foldable t, Eq a) => t a -> a -> Bool
nas = (not .) . has

{-# INLINE pord #-}
pord :: Int -> [a] -> [a]
pord n xs = drop (length xs - n) xs

{-# INLINE isAnyOf #-}
isAnyOf :: Foldable t => t (p -> Bool) -> p -> Bool
isAnyOf fs x = any ($ x) fs

{-# INLINE isAllOf #-}
isAllOf :: Foldable t => t (p -> Bool) -> p -> Bool
isAllOf fs x = all ($ x) fs

{-# INLINE sunder #-}
sunder :: (a -> Bool) -> [a] -> ([a], [a])
sunder = sunder' (w (,) [])

{-# INLINE sunder' #-}
sunder' :: ([a], [a]) -> (a -> Bool) -> [a] -> ([a], [a])
sunder' (goods, bads) _ [] = (goods, bads)
sunder' (goods, bads) f (x:xs) = sunder' sundries f xs
  where
    sundries = if f x then (x:goods, bads) else (goods, x:bads)

{-# INLINE twimap #-}
twimap :: Bifunctor f => (a -> b) -> f a a -> f b b
twimap f = bimap f f

{-# INLINE bind #-}
bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

{-# INLINE shave #-}
shave :: Int -> T.Text -> T.Text
shave n = T.reverse . T.drop n . T.reverse . T.drop n

{-# INLINE shed #-}
shed :: T.Text -> T.Text
shed = shedL . shedR

{-# INLINE shedL #-}
shedL :: T.Text -> T.Text
shedL "" = ""
shedL word = if not (isAlphaNum (T.head word))
  then shedL (T.tail word)
  else word

{-# INLINE shedR #-}
shedR :: T.Text -> T.Text
shedR = T.reverse . shedL . T.reverse

{-# INLINE isNewline #-}
isNewline :: Char -> Bool
isNewline = has ['\n', '\r']

{-# INLINE isLeafroad #-}
isLeafroad :: FilePath -> Bool
isLeafroad = (".hs" ==) . pord 3

{-# INLINE isBranchroad #-}
isBranchroad :: FilePath -> Bool
isBranchroad x = last x == '/'
