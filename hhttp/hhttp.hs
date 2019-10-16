{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Servant
import Servant.HTML.Blaze
import Control.Arrow (second)
import Control.Concurrent
import Text.Blaze.Html
import Text.Hamlet
import Data.FileEmbed
import System.Environment
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsChain, onInsecure, OnInsecure(..))
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.Maybe
import Control.Monad
import System.Directory
import Servant.Multipart
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BS
import System.Process (runCommand, waitForProcess)
import Options.Applicative

type Api =  "upload" :> Get '[HTML] Html
    :<|>    "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[HTML] Html
    :<|>    Raw

server :: FilePath -> Server Api
server path =   uploadServerGet
        :<|>    uploadServerPost
        :<|>    serveDirectoryFileServer path

handleFiles :: MultipartData Tmp -> IO ()
handleFiles multipart = do
    let fs = files multipart
    unless (null fs) $ do
        let f = head fs
            filename = unpack $ fdFileName f
            tmpname = fdPayload f
        putStrLn tmpname
        filename' <- getUniqueName filename
        putStrLn $ "\ESC[32;1mReceived file: " <> filename <> "\ESC[0m"
        void $ runCommand ("mv '" ++ tmpname ++ "' ./'" ++ filename' ++ "'") >>= waitForProcess
        putStrLn $ "\ESC[33;1mSaved as:      " <> filename' <> "\ESC[0m"

getUniqueName :: FilePath -> IO FilePath
getUniqueName fp = do
    exist <- doesFileExist fp
    if exist
        then findOne fp (1 :: Int)
        else return fp
      where
        findOne fn n = do
            exist' <- doesFileExist $ fn <> "." <> show n
            if exist'
                then findOne fp (n + 1)
                else return $ fp <> "." <> show n

uploadServerPost :: MultipartData Tmp -> Handler Html
uploadServerPost multipart = do
    liftIO $ handleFiles multipart
    return [shamlet|
$doctype 5
<p>OK
|]

css :: Text
css = decodeUtf8 $(embedFile "bulma.min.css")

pattern :: Text
pattern = decodeUtf8 . ("data:image/png; base64," <>) . encode $
    $(embedFile "pattern.png") -- stolen from haskell.org

uploadServerGet :: Handler Html
uploadServerGet = return [shamlet|
$newline never
$doctype 5
<html lang=en>
  <head>
    <meta charset=utf-8>
    <meta name=viewport content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta http-equiv=x-ua-compatible content=ie=edge>
    <style>#{css}
    <style>
      .box{background-image:url("#{pattern}");}
  <body>
    <div .section>
      <div .container>
        <form method=post enctype=multipart/form-data .box>
          <div .field.has-addons.has-addons-centered>
            <p .control>
              <input type=file .input #file name=file>
            <p .control>
              <button type=submit .button.is-success>Submit
|]

api :: Proxy Api
api = Proxy

app :: FilePath -> Application
app = gzip def .  logStdout . serve api . server

data Opt = Opt
    { port :: Int
    , dir :: FilePath
    , auth :: Maybe (ByteString, ByteString)
    } deriving Show

extractAuth :: String -> Maybe (ByteString, ByteString)
extractAuth "" = Nothing
extractAuth s = let (u, p) = second BS.tail . BS.break (== ':') $ pack s in
                    if BS.null u && BS.null p
                      then Nothing
                      else Just (u, p)

opts :: Parser Opt
opts = Opt <$> option auto (short 'p' <> long "port" <> value 3000 <>
                            showDefault <> help "specify port")
           <*> strOption (short 'd' <> long "dir" <> value "." <>
                          showDefault <> help "specify directory")
           <*> (extractAuth <$> (option str $ short 'a' <> long "auth" <>
                value "" <> help "basic auth in format 'abc:def'"))

main :: IO ()
main = do
    opt <- execParser $ info (helper <*> opts)
        (fullDesc <> progDesc "Haskell HTTP static file server")
    let port_ = port opt
        dir_ = dir opt
        auth_ = auth opt
        app' = if isJust auth_
                 then let Just (user, pwd) = auth_ in
                    basicAuth (\u p -> return (u == user && p == pwd)) "hhttp" (app dir_)
                 else app dir_
    certDir <- lookupEnv "CERTSDIR"
    case certDir of
        Nothing -> runSettings (setHost "*6" (setPort port_ defaultSettings)) app'
        Just dir -> do
            let tlsS = tlsSettingsChain (dir ++ "cert.pem")
                        [dir ++ "fullchain.pem"] (dir ++ "privkey.pem")
                tlsS' = tlsS { onInsecure = AllowInsecure }
            if port_ /= 80
              then runTLS tlsS' (setHost "*6" (setPort port_ defaultSettings)) $
                    forceSSL app'
              else do
                void . forkIO $ runTLS tlsS'
                    (setHost "*6" $ setPort 80 defaultSettings) $ forceSSL app'
                runTLS tlsS (setHost "*6" $ setPort 443 defaultSettings) app'
