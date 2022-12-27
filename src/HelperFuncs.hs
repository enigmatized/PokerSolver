{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts #-}

{-# Language PatternSynonyms #-}



module HelperFuncs where

import DealCards
import Data.List
import Test.QuickCheck
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map                 as Map
import CardTypes
import Data.Function (on)
import Data.Array.IO
import Data.List
import System.Random
import Test.QuickCheck
import Data.Foldable
import Control.Applicative
import Control.Monad
import System.Environment
import Data.Array.IO
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map                 as Map
import Data.List.Split 






expectedValueIsh :: (Float, Float, Float) -> (Float, Float) -> Float 
expectedValueIsh  (a, b, _) (c, d) = (a * (c)) + (b * ( d)) 


removeCardsFromDeck :: Deck' -> Deck' -> Deck'
removeCardsFromDeck  ((a,b):xs) deck  =  removeCardsFromDeck xs $ removeCard' deck a b
removeCardsFromDeck  []    deck       = deck

makeDeckFromCommmunity :: [String] -> [(Char, Int)]
makeDeckFromCommmunity (y:(x:xs)) = [(head y, (read x :: Int))] ++ (makeDeckFromCommmunity xs)
makeDeckFromCommmunity []         =   []      

readString :: Int -> [String] ->  IO [String] 
readString 0 input = return input
readString n input = do
  z <- getLine
  readString (n-1) (z:input)


addTriplesMultiPair :: ( (Int, Int, Int), (Int, Int, Int) ) -> ( (Int, Int, Int), (Int, Int, Int) ) -> ( (Int, Int, Int), (Int, Int, Int) ) 
addTriplesMultiPair ( a, b) ( c, d) = ((addTriples a c),   (addTriples c d))

addTriples :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTriples (!aaa, !bbb, !ccc) (!ddd, !eee, !fff) = (aaa + ddd, bbb + eee, ccc +fff) 

shuffle'' :: [a] -> IO [a]
shuffle'' xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs



probWinning :: (Int, Int, Int) -> Float
probWinning (a1, a2, a3) =  (fromIntegral a1) / (fromIntegral (a1 + a2 + a3))

showWinRates :: (Int, Int, Int) -> (Float, Float, Float)
showWinRates (a1, a2, a3) = ((fromIntegral a1) / (fromIntegral (a1 + a2 + a3)),  (fromIntegral a2) / (fromIntegral (a1 + a2 + a3) ) ,  (fromIntegral a3) / (fromIntegral (a1 + a2 + a3)) )


properEnding1 :: Int -> String
properEnding1 0 = " FLOP "
properEnding1 3 = " Turn "
properEnding1 4 = " River "
properEnding1 5 = " NO VALUE"
properEnding1 _ = " SOMETHING IS WRONG" 

properEnding2 :: Int -> String
properEnding2 0 = " River "
properEnding2 3 = " River "
properEnding2 4 = " River "
properEnding2 5 = " NO VALUE"
properEnding2 _ = " SOMETHING IS WRONG" 
