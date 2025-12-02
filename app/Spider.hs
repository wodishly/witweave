module Spider
  ( module Spider,
    module Control.Lens,
    module Data.Bifunctor,
    module Data.Char,
    module Data.Foldable,
    module Data.Function,
    module Data.Functor,
    module Data.List,
    module Data.Maybe,
    module GHC.Exts,
    module System.Directory,
  )
where

import Control.Lens ((.~), (^.))
import Data.Bifunctor (Bifunctor (..))
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Colour.SRGB (Colour, sRGB24read)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersect)
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe)
import Debug.Trace (trace)
import GHC.Exts (groupWith)
import System.Directory (listDirectory)

import qualified Data.Text as T (Text)


type Twain a = (a, a)
type Income = T.Text
type Yoke = Twain T.Text

data Hand = L | R deriving (Eq, Show)

data Writ = Writ {
  name :: T.Text,
  incomes :: [Income],
  wits :: [Wit],
  yokes :: [Yoke]
} deriving (Eq, Show)

data Wit = Wit {
  name :: T.Text,
  kind :: T.Text,
  body :: T.Text,
  outTell :: Int,
  inTell :: Int
} deriving Eq

instance Show Wit where
  show Wit { name, kind } = "(" ++ show name ++ ", " ++ show kind ++ ", …)"

ly :: (Show a) => a -> a
ly = ly' id

ly' :: (Show a) => (b -> a) -> b -> b
ly' f x = trace (show (f x)) x

ww :: (a -> a -> b) -> a -> b
ww f x = f x x

sunder :: (a -> Bool) -> [a] -> ([a], [a])
sunder = sunder' (ww (,) [])

sunder' :: ([a], [a]) -> (a -> Bool) -> [a] -> ([a], [a])
sunder' (goods, bads) _ [] = (goods, bads)
sunder' (goods, bads) f (x:xs) = sunder' sundries f xs
  where
    sundries = if f x then (x:goods, bads) else (goods, x:bads)

pord :: Int -> [a] -> [a]
pord n xs = drop (length xs - n) xs

has :: (Foldable t, Eq a) => t a -> a -> Bool
xs `has` x = x `elem` xs

isAnyOf :: Foldable t => t (p -> Bool) -> p -> Bool
isAnyOf fs x = any ($ x) fs

lwhelk :: Colour Double
lwhelk = sRGB24read "#ccccff"

dwhelk :: Colour Double
dwhelk = sRGB24read "#363c50"
