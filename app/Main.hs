{-# LANGUAGE OverloadedStrings #-}

module Main where

import TSLAQPrices (updatePrices)
import Types
import System.Directory (doesFileExist)
import Data.Text
import Control.Lens ((<&>), set)
import Control.Monad.Reader
-- import Control.Monad.Trans.AWS ( AWST', runAWST)
-- import Control.Monad.Trans.Resource ( ResourceT)
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Data.ByteString (ByteString)
import Control.Monad.Trans.AWS (
    Credentials(..)
  , Env
  , LogLevel(..)
  , Region(..)
  , Service
  , envLogger
  , newEnv
  , newLogger
  -- , reconfigure
  -- , runResourceT
  -- , setEndpoint
  -- , within
  )
import System.Log.Logger
-- import System.Log.Handler.Syslog
-- import System.Log.Handler.Simple
-- import System.Log.Handler (setFormatter)
-- import System.Log.Formatter
import Network.AWS.SecretsManager
import Network.AWS.S3
-- import qualified Network.AWS.Types as AWSTypes
import System.IO (stdout)

tslaqPricesLogger :: IO Logger
tslaqPricesLogger = do
  l <- getLogger "main"
  updateGlobalLogger "main" (setLevel DEBUG)
  return l

awsRegion :: Region
awsRegion = NorthVirginia

getEnv :: Bool -> IO Control.Monad.Trans.AWS.Env
getEnv b = do
  l <- newLogger Debug stdout
  case b of
    True ->
      newEnv (FromFile "tslaq-user" "~/.aws/credentials") <&> set envLogger l
    False -> newEnv Discover <&> set envLogger l

getAWSInfo :: IO AWSInfo
getAWSInfo = do
  b <- doesFileExist "~/.aws/credentials"
  e <- getEnv b
  let s3Service      = s3
  let secretsService = secretsManager
  return $ AWSInfo e awsRegion s3Service secretsService

main :: IO ()
main = do
  awsInfo <- getAWSInfo
  l       <- tslaqPricesLogger
  let env = Env {envLog = l, envAWSInfo = awsInfo}
  runReaderT (updatePrices) env
