import Network.Curl ( CurlCode
                    , curlGetString)

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

server = "http://127.0.0.1:7379/"

data CustomSet = CustomSet { key :: String
                           , value :: String
                           } deriving (Show)

get :: [Char] -> IO [Char]
get key = do
        (a,b) <- curlGetString (server ++ "GET/" ++ key) []
        return $ drop 8 $ take (length b - 2) b

set :: CustomSet -> IO (CurlCode, String)
set customSet = curlGetString (server ++ "SET/" ++ key customSet ++ "/" ++ value customSet) []

flushall :: IO (CurlCode, String)
flushall = curlGetString (server ++ "FLUSHALL") []

instance Arbitrary CustomSet where
  arbitrary = do
  key <- listOf1 genSafeChar
  value <- listOf1 genSafeChar
  return (CustomSet key value)
  where
    genSafeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

getsetHasExpectedBehavior :: CustomSet -> Property
getsetHasExpectedBehavior customSet = monadicIO $ do
  _ <- run $ set customSet
  _ <- run $ threadDelay 250000
  realityMatchesModel <- run $ ioStringsAreEqual first second
  assert realityMatchesModel
  where
    first = (return $ value customSet)
    second = get $ key customSet
    ioStringsAreEqual s1 s2 = (==) <$> s1 <*> s2

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000000 } )

main = do
  flushall
  quickCheckWith customArgs getsetHasExpectedBehavior
