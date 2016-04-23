{-# LANGUAGE OverloadedStrings #-}

import Database.Redis ( Connection
                      , RedisCtx
                      , runRedis
                      , get
                      , set
                      , flushall
                      , connect
                      , defaultConnectInfo
                      , zadd
                      , zrangeWithscores
                      , zrem
                      , zremrangebyrank
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

import Control.Monad.Trans ( liftIO )

import Data.ByteString.Char8 ( ByteString
                             , pack
                             )

data CustomSet = CustomSet { score :: Double
                           , value :: ByteString
                           } deriving (Show)

instance Arbitrary CustomSet where
 arbitrary = do
 score <- arbitrary
 value <- listOf1 genSafeChar
 return ( CustomSet score (pack $ value) )
 where
   genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sortedSetName = "sset"

add :: RedisCtx m f => CustomSet -> m (f Integer)
add customSet = zadd sortedSetName [(score customSet, value customSet)]

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

sortedSetHasExpectedBehavior :: Connection -> CustomSet -> Property
sortedSetHasExpectedBehavior conn customSet = monadicIO $ do
  _ <- run $ do
    threadDelay 500000
    runRedis conn $ add customSet

    getScore <- runRedis conn $ do
      val <- zrangeWithscores sortedSetName 0 (-1)
      case val of
        Left _ -> return $ 0.0
        Right v -> return $ snd $ head $ v

    liftIO $ print getScore
  assert $ 1 < 2

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  _ <- runRedis conn $ zremrangebyrank sortedSetName 0 (-1)
  quickCheckWith customArgs (sortedSetHasExpectedBehavior conn)
