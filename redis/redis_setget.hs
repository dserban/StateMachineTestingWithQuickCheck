{-# LANGUAGE OverloadedStrings #-}

import Database.Redis ( Connection
                      , runRedis
                      , get
                      , set
                      , flushall
                      , connect
                      , defaultConnectInfo
                      )

import Test.QuickCheck ( Arbitrary
                       , Args
                       , Gen
                       , Property
                       , arbitrary
                       , maxSuccess
                       , quickCheckWith
                       , stdArgs
                       , elements
                       , listOf1
                       )

import Test.QuickCheck.Monadic ( assert
                               , monadicIO
                               , run
                               )

import Control.Applicative ( (<$>)
                           , (<*>)
                           )

import Control.Concurrent ( threadDelay )

import Data.Maybe ( fromMaybe )

import Data.ByteString.Char8 ( ByteString
                             , pack
                             )

data CustomSet = CustomSet { key :: ByteString
                           , value :: ByteString
                           } deriving (Show)

instance Arbitrary CustomSet where
  arbitrary = do
  key <- listOf1 genSafeChar
  value <- listOf1 genSafeChar
  return ( CustomSet (pack $ key) (pack $ value) )
  where
    genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getsetHasExpectedBehavior :: Connection -> CustomSet -> Property
getsetHasExpectedBehavior conn customSet = monadicIO $ do
  _ <- run $ do
    runRedis conn $ set (key customSet) (value customSet)
  realityMatchesModel <- run $ ioStringsAreEqual first second
  assert realityMatchesModel
  where
    first = (return $ value customSet)
    second = do
      runRedis conn $ do
        val <- get $ key customSet
        case val of
          Left _ -> return "Some error occurred"
          Right v -> return $ fromMaybe "Could not find key in store" v
    ioStringsAreEqual s1 s2 = (==) <$> s1 <*> s2

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  runRedis conn $ flushall
  quickCheckWith customArgs (getsetHasExpectedBehavior conn)
