{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Lens              ((&), (.~))
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS   (Credentials (..), Region (..))
import           Data.Maybe                (fromMaybe)
import           Network.AWS.Easy          (AWSConfig, Endpoint (..), awsConfig,
                                            awscCredentials, connect)
import           System.Console.GetOpt
import           System.Directory          (doesFileExist)
import           System.Environment
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (Logger, Priority (..), addHandler,
                                            getLogger, removeAllHandlers,
                                            setLevel, updateGlobalLogger)
import           TSLAQPrices               (getLatestJSONFile,
                                            localPricesFolder, updatePrices)
import           Types                     (Env (..), s3Service,
                                            secretsManagerService)

tslaqPricesLogger :: IO Logger
tslaqPricesLogger = do
  l <- getLogger "main"
  h <- fileHandler (localPricesFolder ++ "debug.log") DEBUG >>= \lh ->
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

data Options = Options
  { optGetLatest :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {optGetLatest = False}

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "l"
           ["latest"]
           (NoArg $ \o -> return o { optGetLatest = True })
           "Get latest price file"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optGetLatest = latest } = opts
  l              <- tslaqPricesLogger
  c              <- getAWSConfig
  s3Session      <- connect c s3Service
  secretsSession <- connect c secretsManagerService
  let env =
        Env {envLog = l, s3Session = s3Session, secretsSession = secretsSession}
  case latest of
    True -> do
      l <- getLatestJSONFile
      let l' = fromMaybe "" l
      putStr l'
    False -> runReaderT updatePrices env
  removeAllHandlers
