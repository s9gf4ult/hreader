module Main where

import Control.Monad.Base
import Control.Monad.HReader
import Data.HSet

type ListA = '[Int, Bool, Char]

type ListB = '[Int, Bool]

type MonadA m =
  ( MHRElemsConstraint m ListA
  , MonadHReader m
  , MonadBase IO m
  )

type MonadB m =
  ( MHRElemsConstraint m ListB
  , MonadHReader m
  , MonadBase IO m
  )

printInt
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Int)
  => m ()
printInt = do
  int <- hask
  liftBase $ print $ "int: " ++ show (int :: Int)

printBool
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Bool)
  => m ()
printBool = do
  b <- hask
  liftBase $ print $ "bool: " ++ show (b :: Bool)

printChar
  :: (MonadBase IO m, MonadHReader m, HGettable (MHRElements m) Char)
  => m ()
printChar = do
  c <- hask
  liftBase $ print $ "char: " ++ show (c :: Char)

monadA :: (MonadA m) => m ()
monadA = do
  liftBase $ print "monad A"
  printInt
  printBool
  printChar
  monadB

monadB :: (MonadB m) => m ()
monadB = do
  liftBase $ print "monad B"
  printInt
  printBool

runB :: (AllHGettable els ListB) => HSet els -> IO ()
runB h = do
  print "performing some IO actions around B"
  runHReaderT h monadB

runBinA :: (MonadA m) => m ()
runBinA = do
  h <- askHSet
  liftBase $ runB h

main :: IO ()
main = do
  let
    ha :: HSet ListA
    ha = HSCons 10 $ HSCons True $ HSCons 'c' HSNil
    hb :: HSet ListB
    hb = subHSet ha
  runHReaderT ha monadA
  runHReaderT ha monadB
  runHReaderT hb monadB
  runHReaderT ha runBinA
