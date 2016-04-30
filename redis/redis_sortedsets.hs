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

import System.Random ( StdGen
                     , getStdGen
                     , randomR
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
  realityMatchesModel <- run $ do
    threadDelay 500000
    runRedis conn $ add customSet

    setRange <- runRedis conn $ do
      val <- zcard sortedSetName
      case val of
        Left _ -> return 0
        Right v -> return v

    randomGen <- getStdGen

    let (randNumber1, newGen1) = getRandomPair 1 setRange randomGen
    let (randNumber2, newGen2) = getRandomPair randNumber1 setRange newGen1
    let (randNumber3, newGen3) = getRandomPair randNumber2 setRange newGen2

    testEntry1 <- getEntry (randNumber1-1)
    testEntry2 <- getEntry (randNumber2-1)
    testEntry3 <- getEntry (randNumber3-1)

    liftIO $ print $ (show testEntry1) ++ " " ++ (show testEntry2) ++ " " ++ (show testEntry3)

    return $ (testEntry1 <= testEntry2) && (testEntry2 <= testEntry3)

  assert $ realityMatchesModel

  where
    getEntry x = runRedis conn $ do
      val <- zrangeWithscores sortedSetName x x
      case val of Right v -> return $ snd $ head $ v
    getRandomPair x y gen = randomR (x, y) gen :: (Integer, StdGen)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  _ <- runRedis conn $ zremrangebyrank sortedSetName 0 (-1)
  quickCheckWith customArgs (sortedSetHasExpectedBehavior conn)
