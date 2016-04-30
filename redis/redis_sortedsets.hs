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
                      , zcard
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

import System.Random

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
  realityMatchesModel <- run $ do
    --threadDelay 500000
    runRedis conn $ add customSet

    setRange <- runRedis conn $ do
      val <- zcard sortedSetName
      case val of
        Left _ -> return 0
        Right v -> return v

    randomGen <- getStdGen

    let (randNumber1, newGen1) = randomR (1, setRange) randomGen :: (Integer, StdGen)
    let (randNumber2, newGen2) = randomR (randNumber1, setRange) newGen1 :: (Integer, StdGen)
    let (randNumber3, newGen3) = randomR (randNumber2, setRange) newGen2 :: (Integer, StdGen)

    testEntry1 <- runRedis conn $ do
      val <- zrangeWithscores sortedSetName (randNumber1-1) (randNumber1-1)
      case val of Right v -> return $ snd $ head $ v

    testEntry2 <- runRedis conn $ do
      val <- zrangeWithscores sortedSetName (randNumber2-1) (randNumber2-1)
      case val of Right v -> return $ snd $ head $ v

    testEntry3 <- runRedis conn $ do
      val <- zrangeWithscores sortedSetName (randNumber3-1) (randNumber3-1)
      case val of Right v -> return $ snd $ head $ v

    liftIO $ print $ (show testEntry1) ++ " " ++ (show testEntry2) ++ " " ++ (show testEntry3)

    return $ (testEntry1 <= testEntry2) && (testEntry2 <= testEntry3)

  assert $ realityMatchesModel

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  _ <- runRedis conn $ zremrangebyrank sortedSetName 0 (-1)
  quickCheckWith customArgs (sortedSetHasExpectedBehavior conn)
