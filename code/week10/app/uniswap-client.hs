{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                           (when)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Aeson                              (Result (..), ToJSON, decode, fromJSON)
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid                             (Last (..))
import           Data.Proxy                              (Proxy (..))
import           Data.String                             (IsString (..))
import           Data.Text                               (Text, pack)
import           Data.UUID                               hiding (fromString)
import           Ledger.Value                            (AssetClass (..), CurrencySymbol, flattenValue)
import           Network.HTTP.Req
import qualified Plutus.Contracts.Uniswap                as US
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Plutus.PAB.Webserver.Types
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)
import           Text.Read                               (readMaybe)
import           Wallet.Emulator.Types                   (Wallet (..))

import           Uniswap                                 (cidFile, UniswapContracts)

main :: IO ()
main = do
    w   <- Wallet . read . head <$> getArgs
    cid <- read                 <$> readFile (cidFile w)
    mcs <- decode               <$> LB.readFile "symbol.json"
    case mcs of
        Nothing -> putStrLn "invalid symbol.json" >> exitFailure
        Just cs -> do
            putStrLn $ "cid: " ++ show cid
            putStrLn $ "symbol: " ++ show (cs :: CurrencySymbol)
            go cid cs
  where
    go :: UUID -> CurrencySymbol -> IO a
    go cid cs = do
        cmd <- readCommandIO
        case cmd of
            Funds                    -> getFunds cid
            Pools                    -> getPools cid
            Create amtA tnA amtB tnB -> createPool cid $ toCreateParams cs amtA tnA amtB tnB
        go cid cs

data Command =
      Funds
    | Pools
    | Create Integer Char Integer Char
    deriving (Show, Read, Eq, Ord)

readCommandIO :: IO Command
readCommandIO = do
    putStrLn "Enter a command: Funds, Pools, Create amtA tnA amtB tnB"
    s <- getLine
    maybe readCommandIO return $ readMaybe s

toCreateParams :: CurrencySymbol -> Integer -> Char -> Integer -> Char -> US.CreateParams
toCreateParams cs amtA tnA amtB tnB = US.CreateParams (toCoin tnA) (toCoin tnB) (US.Amount amtA) (US.Amount amtB)
  where
    toCoin :: Char -> US.Coin c
    toCoin tn = US.Coin $ AssetClass (cs, fromString [tn])

getFunds :: UUID -> IO ()
getFunds cid = do
    callEndpoint cid "funds" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (US.Funds v) -> putStrLn $ "funds: " ++ show (flattenValue v)
            _                  -> go

getPools :: UUID -> IO ()
getPools cid = do
    callEndpoint cid "pools" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (US.Pools ps) -> putStrLn $ "pools: " ++ show ps
            _                   -> go

createPool :: UUID -> US.CreateParams -> IO ()
createPool cid cps = do
    callEndpoint cid "create" cps
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right US.Created -> putStrLn "created"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go

getStatus :: UUID -> IO (Either Text US.UserContractState)
getStatus cid = runReq defaultHttpConfig $ do
    w <- req
        GET
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState UniswapContracts)))
        (port 8080)
    case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (Last Nothing)  -> liftIO $ threadDelay 1_000_000 >> getStatus cid
        Success (Last (Just e)) -> return e
        _                       -> liftIO $ ioError $ userError "error decoding state"

callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()
callEndpoint cid name a = handle h $ runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show cid) /: "endpoint" /: pack name)
        (ReqBodyJson a)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    when (responseStatusCode v /= 200) $
        liftIO $ ioError $ userError $ "error calling endpoint " ++ name
  where
    h :: HttpException -> IO ()
    h = ioError . userError . show
