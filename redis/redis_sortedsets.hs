{-# LANGUAGE OverloadedStrings #-}

import Database.Redis ( Connection
                      , runRedis
                      , get
                      , set
                      , flushall
                      , connect
                      , defaultConnectInfo
                      , zadd
                      , zrem
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

sortedSetName = "sset"

add score val = do
  _ <- zadd sortedSetName [(score,val)]
  return ()
