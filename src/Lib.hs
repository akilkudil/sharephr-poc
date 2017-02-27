{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Jsonp
import Text.Hamlet (shamlet)
import Servant.HTML.Blaze 
import Text.Blaze.Html5 hiding (main)
import Network.Wai.Middleware.Cors
import Text.Lucius (CssUrl, luciusFile, luciusFileReload, renderCss, renderCssUrl)
import qualified Data.Text.Lazy.IO as TLIO




data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)



template = $(luciusFile "template.lucius") 
{-
cssStyle :: Html 
cssStyle = toHtml $ renderCssUrl undefined 
    template
-}

cssStyle = toHtml $ renderCss $ template undefined


data LoginPage = LoginPage 

instance ToMarkup LoginPage where
      toMarkup LoginPage = builderHtml       

builderHtml = [shamlet|
                $doctype 5
                <html>
                    <head>
                        <title>Greeting2 #{userFirstName person}
                        <style>#{cssStyle}
                    <body>
                        <h2> Hello world HTML #{userFirstName person} #{userLastName person} |] 
                                                 where
                                                    person = albert



type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
           :<|> "mypage" :> Get '[HTML] Html 
           :<|> "haml" :> Get '[HTML] LoginPage

startApp :: IO ()
startApp = run 12000 app

app :: Application
app = simpleCors (serve api server2)

api :: Proxy UserAPI2
api = Proxy

server2 :: Server UserAPI2
server2 = return users
      :<|> return albert
      :<|> return isaac
      :<|> return page
      :<|> return LoginPage


page::Html
page = p "Hello world"

isaac::User
isaac = User 1 "Isaac" "Newton"

albert::User
albert = User 2 "Albert" "Einstein"

users :: [User]
users = [ isaac, albert]

