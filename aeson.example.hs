{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Maybe
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL

data Struct = Struct { x :: Int, y :: Double } deriving (Show, Generic)

instance FromJSON Struct
instance ToJSON Struct

main :: IO ()
main = do
  let to   = decode "{\"y\":3.14,\"x\": 1}" :: Maybe Struct
  let from = encode $ Struct 142 2.71

  print $ fromJust to
  BL.putStrLn from
