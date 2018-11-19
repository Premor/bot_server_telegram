{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Lib
import Data.Aeson
import Web.Scotty as SC

import Data.Monoid    (mconcat)
import Web.Scotty.TLS

import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Control.Applicative
import Control.Monad.IO.Class
import qualified Network.WebSockets as WS
import GHC.Generics
-- import qualified Data.Configurator as C
-- import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy as TL
import Wuss
-- data Test = Test {id::Int,name::String} deriving Generic

-- instance ToJSON Test

data Deribit = Deribit {id::Int,action::String} deriving Generic
instance ToJSON Deribit

-- main :: IO ()
-- main = scottyTLS 443 "bot_private.key" "bot_public.pem" $ do
--          get "/:word" $ do
--              beam <- param "word"
--              json



-- main :: IO ()
-- main = do
-- 	let port = 3000
-- 	let settings = Warp.setPort port Warp.defaultSettings
-- 	sapp <- scottyApp
-- 	Warp.runSettings settings $ WaiWs.websocketsOr WS.defaultConnectionOptions wsapp sapp

-- scottyApp :: IO Wai.Application
-- scottyApp = SC.scottyApp $ do
-- 	middleware logStdoutDev
-- 	get "/test" $ SC.json $ Test 1 "qwe"

-- wsapp :: WS.ClientApp
-- wsapp pending = do
--   putText "ws connected"
--   conn <- WS.acceptRequest pending
--   WS.forkPingThread conn 30

--   (msg :: Text) <- WS.receiveData conn
--   WS.sendTextData conn $ ("initial> " :: Text) <> msg

--   forever $ do
--     WS.sendTextData conn $ "loop data"
--     threadDelay $ 1 * 1000000

main :: IO ()
main = runSecureClient "test.deribit.com" 443 "/ws/api/v1/" ws

ws :: WS.ClientApp ()
ws connection = do
    putStrLn "Connected"
    let mess = encode $ Deribit 3035 "/api/v1/public/test"
    print mess
    WS.sendTextData connection mess
    void . forever $ do
        -- message <- WS.receiveData connection
        -- print (message::TL.Text)
		

        message <- WS.receiveData connection
        print (message::TL.Text)

--     let loop = do
--             line <- getLine
--             unless (null line) $ do
--                 WS.sendTextData connection (TL.pack line)
--                 loop
--     loop

    --WS.sendClose connection ("Bye!"::TL.Text)

