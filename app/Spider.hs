module Spider where

import Debug.Trace (trace)
import qualified Data.Text as T (Text)


data Wit = Wit {
  name :: T.Text,
  kind :: T.Text,
  body :: T.Text,
  outs :: Int,
  ins :: Int
} deriving (Eq, Show)

type Yell = (T.Text, T.Text)

ly :: Show a => a -> a
ly = ly' id

ly' :: Show a => (b -> a) -> b -> b
ly' f x = trace (show (f x)) x

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

sunder :: (a -> Bool) -> [a] -> ([a], [a])
sunder f xs = sunder' f xs ([], [])

sunder' :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
sunder' _ [] (goods, bads) = (goods, bads)
sunder' f (x:xs) (goods, bads) = sunder' f xs sundries
  where sundries = if f x then (x:goods, bads) else (goods, x:bads)

(!?) :: [a] -> Int -> Maybe a
[]!?_ = Nothing
(x:_)!?0 = Just x
(_:xs)!?n = xs!?pred n
