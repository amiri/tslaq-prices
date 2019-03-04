{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Lens              ((&), (.~))
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS   (Credentials (..), Region (..))
import           Network.AWS.Easy          (AWSConfig, Endpoint (..), awsConfig,
                                            awscCredentials, connect)
import           System.Directory          (doesFileExist)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (Logger, Priority (..), addHandler,
                                            getLogger, removeAllHandlers,
                                            setLevel, updateGlobalLogger)
import           TSLAQPrices               (updatePrices)
import           Types                     (Env (..), s3Service,
                                            secretsManagerService)

tslaqPricesLogger :: IO Logger
tslaqPricesLogger = do
  l <- getLogger "main"
  h <- fileHandler "/var/local/tslaq-prices/debug.log" DEBUG >>= \lh ->
    return $ setFormatter
      lh
      (simpleLogFormatter "[$time - $loggername - $prio] $msg")
  updateGlobalLogger "main" (setLevel DEBUG)
  updateGlobalLogger "main" (addHandler h)
  getLogger "main"

awsRegion :: Region
awsRegion = NorthVirginia

getCredentials :: Bool -> Credentials
getCredentials b = do
  case b of
    True  -> FromFile "tslaq-user" "/home/amiri/.aws/credentials"
    False -> Discover

getAWSConfig :: IO AWSConfig
getAWSConfig = do
  b <- doesFileExist "/home/amiri/.aws/credentials"
  let creds = getCredentials b
  let r     = AWSRegion awsRegion
  let c     = awsConfig r & awscCredentials .~ creds
  return c

main :: IO ()
main = do
  l              <- tslaqPricesLogger
  c              <- getAWSConfig
  s3Session      <- connect c s3Service
  secretsSession <- connect c secretsManagerService
  let env =
        Env {envLog = l, s3Session = s3Session, secretsSession = secretsSession}
  runReaderT updatePrices env
  removeAllHandlers
