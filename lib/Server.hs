{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Data.Foldable (forM_)
import GHC.Conc.Sync (atomically)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Web.Scotty (middleware, scotty, ScottyM, ActionM, get, html, json, param)
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))

import Config
import WeatherWar

home :: TVar [HighScore] -> ScottyM ()
home tv = get "/" $ do
  tv' <- liftIO (readTVarIO tv)
  Web.Scotty.html $ R.renderHtml $ do
    H.docTypeHtml $ do
      H.head $ do
        H.title "Weather Wars"
      H.body $ do
        H.div ! A.style "text-align:left; display:inline-block;" $
          H.ol ! A.type_ "1" $ 
            forM_ tv' (\(HS (a,b)) -> H.li (H.toHtml (show a ++ " - " ++ show b))) 

runServer :: ServerConfig -> TVar [HighScore] -> IO ()
runServer cfg tv =
    scotty port $ do 
      middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
      middleware logStdoutDev 
      home tv
  where
    port = (fromIntegral . srvPort) cfg
