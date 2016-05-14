{-# LANGUAGE OverloadedStrings #-}

import Database.Redis ( Connection
                      , RedisCtx
                      , Reply
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
                       , choose
                       , maxSuccess
                       , quickCheckWith
                       , stdArgs
                       , elements
                       , listOf1
                       , vectorOf
                       )

import Test.QuickCheck.Monadic ( assert
                               , monadicIO
                               , run
                               )

import Control.Monad.Trans ( liftIO )

import Data.ByteString.Char8 ( ByteString
                             , pack
                             , unpack
                             )

import System.Random ( StdGen
                     , getStdGen
                     , randomR
                     )

import Control.Applicative ( (<$>) )

import Foreign.C.Math.Double ( fabs )

import Debug.Trace

import Control.DeepSeq

data Action = Add | Delete deriving (Show, Eq)

data CustomSet = MkCustomSet { score :: Double
                             , value :: ByteString
                             , action :: Action
                             } deriving (Show)

data CustomSetList = MkCustomSetList [CustomSet]
                     deriving (Show)

instance Arbitrary Action where
  arbitrary = do
    rand <- arbitrary
    if rand == True then
      return Add
    else
      return Delete

instance Arbitrary CustomSet where
 arbitrary = do
   score <- arbitrary
   value <- listOf1 genSafeChar
   action <- arbitrary
   return $ MkCustomSet score (pack $ value) action
  where
   genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary CustomSetList where
  arbitrary = do
    list <- listOf1 (arbitrary :: Gen CustomSet)
    return $ MkCustomSetList list

sortedSetName :: ByteString
sortedSetName = "sset"

add :: RedisCtx m f => CustomSet -> m (f Integer)
add customSet = if fabs (score customSet) < 0.1
                then zadd sortedSetName [(0, value customSet)]
                else zadd sortedSetName [(score customSet, value customSet)]

delete :: RedisCtx m f => CustomSet -> m (f Integer)
delete customSet = zrem sortedSetName [(value customSet)]

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

sortedSetHasExpectedBehavior :: Connection -> CustomSetList -> Property
sortedSetHasExpectedBehavior conn customSetList = monadicIO $ do
  realityMatchesModel <- run $ do
    let MkCustomSetList csl = customSetList

    _ <- mymap csl

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

    {-writeFile "redis_sortedsets.log" $ (show $ action customSet) ++ ": " ++ (unpack $ value customSet) ++
                                      "\n" ++ "Indexes chosen: " ++ (show $ randNumber1-1) ++ " "
                                      ++ (show $ randNumber2-1) ++ " " ++ (show $ randNumber3-1) ++ "\n" ++
                                      "Scores: " ++ (show testEntry1) ++ " " ++ (show testEntry2)
                                      ++ " " ++ (show testEntry3) ++ "\n\n"-}

    return $ (testEntry1 <= testEntry2) && (testEntry2 <= testEntry3)

  assert $ realityMatchesModel

  where
    mymap [] = do
      return 0

    mymap li = do
      mapAction $ head li
      _ <- mymap $ tail li
      return 0

    mapAction :: CustomSet -> IO (Either Reply Integer)
    mapAction customSet = do
      case action customSet of
        Add -> runRedis conn $ add customSet
        Delete -> runRedis conn $ delete customSet

    getEntry x = runRedis conn $ do
      val <- zrangeWithscores sortedSetName x x
      case val of
        Right v -> if v == [] then return 0
                              else return $ snd $ head $ v

    getRandomPair x y gen = randomR (x, y) gen :: (Integer, StdGen)

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  _ <- runRedis conn $ zremrangebyrank sortedSetName 0 (-1)
  quickCheckWith customArgs (sortedSetHasExpectedBehavior conn)
