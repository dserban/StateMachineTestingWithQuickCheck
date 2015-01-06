import qualified Data.ByteString.Lazy.Char8 as LC

import Test.QuickCheck         ( Arbitrary
                               , Args
                               , Gen
                               , Property
                               , arbitrary
                               , choose
                               , frequency
                               , maxSuccess
                               , quickCheckWith
                               , shrink
                               , stdArgs
                               , vectorOf
                               )
import Test.QuickCheck.Monadic ( assert
                               , monadicIO
                               , run
                               )
import Control.Applicative     ( (<$>) )
import Network.HTTP.Conduit    ( httpLbs
                               , parseUrl
                               , responseBody
                               , simpleHttp
                               , urlEncodedBody
                               , withManager
                               )
import Data.List               ( sort
                               , subsequences
                               )

data Action =
  Clear | PopLeft | PushRight Int
    deriving (Eq,Show)

newtype ActionList =
  MkActionList [Action]
    deriving (Eq,Show)

newtype CircularBufferLength =
  MkCircularBufferLength Int
    deriving Show

instance Arbitrary Action where
  arbitrary = frequency actionWeights
    where
      genPushRight :: Gen Action
      genPushRight = do
        elementToPushRight <- choose (10000,99999)
        return . PushRight $ elementToPushRight
      actionWeights :: [(Int, Gen Action)]
      actionWeights = [ ( 2, return Clear)
                      , (11, return PopLeft)
                      , (20, genPushRight)
                      ]

instance Arbitrary ActionList where
  arbitrary = do
    actionListLength <- choose (1,50)
    MkActionList <$> ( vectorOf actionListLength $ (arbitrary :: Gen Action) )
  shrink (MkActionList actionList) =
    let
      ceilingLength = min (length actionList) 3
    in
      map MkActionList
      $ filter ((<ceilingLength) . length)
      $ subsequences actionList

-- Implementation of simplified model starts here
maxLength :: Int
maxLength = 10

emptyCB :: CircularBufferLength
emptyCB = MkCircularBufferLength 0

cbCurrentLength :: CircularBufferLength -> Int
cbCurrentLength (MkCircularBufferLength currentLength) = currentLength

clamp :: Int -> Int -> Int -> Int
clamp lowerBound upperBound i =
  (!! 1) . sort $ [lowerBound, upperBound, i]

popLeft :: CircularBufferLength -> CircularBufferLength
popLeft (MkCircularBufferLength currentLength) =
  MkCircularBufferLength $ clamp 0 maxLength (currentLength - 1)

pushRight :: a -> CircularBufferLength -> CircularBufferLength
pushRight element (MkCircularBufferLength currentLength) =
  MkCircularBufferLength $ clamp 0 maxLength (currentLength + 1)
-- Implementation of simplified model ends here

urlPrefix :: String
urlPrefix = "http://192.168.2.3:5000/"

urlForAction :: Action -> String
urlForAction action = urlPrefix ++ urlSuffix action
  where
    urlSuffix Clear                          = "clear"
    urlSuffix PopLeft                        = "popleft"
    urlSuffix (PushRight elementToPushRight) = "pushright/" ++ show elementToPushRight

functionToApplyForAction :: Action -> (CircularBufferLength -> CircularBufferLength)
functionToApplyForAction Clear                          = const emptyCB
functionToApplyForAction PopLeft                        = popLeft
functionToApplyForAction (PushRight elementToPushRight) = pushRight elementToPushRight

performRequestAndGetRemoteLength :: Action -> IO Int
performRequestAndGetRemoteLength action = do
  _ <- parseUrl (urlForAction action)
       >>= return . httpLbs . urlEncodedBody []
       >>= withManager
       >>= return . responseBody
  response <- simpleHttp $ urlPrefix ++ "cblength"
  let ( Just (pyLength, _) ) = LC.readInt response
  return pyLength

absorbActionIntoCircularBufferStateIO
  :: (CircularBufferLength, Bool)
     -> Action
     -> IO (CircularBufferLength, Bool)
absorbActionIntoCircularBufferStateIO lengthBoolCombo action = do
  let oldState = fst lengthBoolCombo
  let newState = functionToApplyForAction action oldState
  let hsLength = cbCurrentLength newState
  pyLength <- performRequestAndGetRemoteLength action
  let lengthsDoMatch = pyLength == hsLength
  putStrLn ""
  putStrLn ""
  putStrLn $ "Old state:  " ++ show oldState
  putStrLn $ "New action: " ++ show action
  putStrLn $ "New state:  " ++ show newState
  putStrLn $ "HS length:  " ++ show hsLength
  putStrLn $ "PY length:  " ++ show pyLength
  putStrLn $ "Match:      " ++ show lengthsDoMatch
  putStrLn ""
  putStrLn ""
  return (newState, lengthsDoMatch)

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f q1 [] = return [q1]
scanM f q1 (x:xs) = do
  q2 <- f q1 x
  qs <- scanM f q2 xs
  return (q1:qs)

everythingMatches :: ActionList -> IO Bool
everythingMatches (MkActionList actionList) =
  scanM absorbActionIntoCircularBufferStateIO (emptyCB, True) actionList
  >>= return . and . map snd

propCircularBufferHasExpectedBehavior :: ActionList -> Property
propCircularBufferHasExpectedBehavior actionList = monadicIO $ do
  realityMatchesModel <- run $ do
    _ <- performRequestAndGetRemoteLength Clear
    everythingMatches actionList
  assert realityMatchesModel

customArgs :: Args
customArgs = ( stdArgs { maxSuccess = 1000000 } )

main :: IO ()
main = quickCheckWith customArgs propCircularBufferHasExpectedBehavior
