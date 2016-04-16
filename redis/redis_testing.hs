{-# LANGUAGE OverloadedStrings #-}

import Database.Redis

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

import Control.Applicative( (<$>)
                          , (<*>)
                          )

import Control.Concurrent ( threadDelay )

data CustomSet = CustomSet { key :: String
                           , value :: String
                           } deriving (Show)

instance Arbitrary CustomSet where
  arbitrary = do
  key <- listOf1 genSafeChar
  value <- listOf1 genSafeChar
  return (CustomSet key value)
  where
    genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getsetHasExpectedBehavior :: CustomSet -> Property
getsetHasExpectedBehavior customSet = monadicIO $ do
  _ <- run $ do
    runRedis conn $ set (key customSet) (value customSet)
  realityMatchesModel <- run $ ioStringsAreEqual first second
  assert realityMatchesModel
  where
    first = (return $ value customSet)
    second = get $ key customSet
    ioStringsAreEqual s1 s2 = (==) <$> s1 <*> s2

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

main = do
  conn <- connect defaultConnectInfo
  quickCheckWith customArgs getsetHasExpectedBehavior
