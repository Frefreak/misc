{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

import Network.Wreq hiding (Proxy)
import qualified Network.Wreq.Session as S
import Control.Lens.Operators hiding ((.=))
import Servant.Client hiding (responseBody)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.HTTP.Media ((//), (/:))
import Servant

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Monoid
import Control.Monad
import Control.Exception
import Control.Concurrent.Async
import System.Environment

import Data.Attoparsec.Text
import Text.Regex.PCRE.Heavy

import Text.XML
import Text.XML.Cursor

bibtexFormat :: T.Text
bibtexFormat = "download-bibtex"

ieeeApi :: String
ieeeApi = "http://ieeexplore.ieee.org/gateway/ipsSearch.jsp"

citationUrl :: String
citationUrl = "http://ieeexplore.ieee.org/xpl/downloadCitations"

ieeeBaseUrl :: BaseUrl
ieeeBaseUrl = BaseUrl Http "ieeexplore.ieee.org" 80 "/gateway/ipsSearch.jsp"

paper1 :: T.Text
paper1 = "Low-power high-throughput BCH error correction VLSI design for multi-level cell NAND Flash memories"

type API = QueryParam "ti" T.Text :> Get '[XML] BS.ByteString

api :: Proxy API
api = Proxy

queryTitle' = client api

queryTitle :: T.Text -> IO BS.ByteString
queryTitle title = do
    manager <- newManager defaultManagerSettings
    r <- runExceptT (queryTitle' (Just title) manager ieeeBaseUrl)
    case r of
        Left l -> error (show l)
        Right r' -> return r'

data XML = XML BS.ByteString

instance Accept XML where
    contentType _ = "text" // "xml"

instance MimeUnrender XML BS.ByteString where
    mimeUnrender _ bs = return bs

getCitationText :: T.Text -> T.Text -> T.Text -> Int -> IO T.Text
getCitationText format detailUrl recordId num = do
    r <- S.withSession $ \sess -> do
        S.get sess (T.unpack detailUrl)
        S.post sess citationUrl $
            [ "recordIds" := recordId
            , "citations-format" := ("citation-only" :: String)
            , "download-format":= format
            {-, "x" := (0 :: Int)-}
            {-, "y" := (0 :: Int)-}
            ]
    let cit' = gsub [re|<br>|] ("" :: BS.ByteString) (r ^. responseBody)
        cit'' = gsub [re|(\r\n)+|] ("\n" :: BS.ByteString) cit'
        Right reg = compileM (encodeUtf8 recordId) []
        cit''' = sub reg (show num) cit''
    return . decodeUtf8 . BS.toStrict $ cit'''

getBibtexCitation :: T.Text -> Int -> IO T.Text
getBibtexCitation paper num = do
    r <- queryTitle paper
    let doc = parseLBS def r
    case doc of
        Left e -> throw e
        Right doc -> do
            let cursor = fromDocument doc
                mdurl = cursor $// element "mdurl" &// content
            if null mdurl then
                return (T.replicate 80 "*" <> "\n" <> paper <> "\n" <>
                    T.replicate 80 "*" <> "\n") else do
            let recordId = cursor $// element "arnumber" &// content
            T.concat <$> (mapConcurrently (\(url, rid) ->
                getCitationText bibtexFormat url rid num) $ zip mdurl recordId)

parseTitle :: Parser (Int, T.Text)
parseTitle = do
    char '['
    nu <- read <$> many1 digit
    char ']'
    skipWhile (\c -> c /= '"' && c /= '“')
    satisfy (const True)
    title <- Data.Attoparsec.Text.takeWhile (\c -> c /= '"' && c /= '”')
    return (nu, title)

parseTitle' :: T.Text -> (Int, T.Text)
parseTitle' l = case parseOnly parseTitle l of
                    Left err -> error err
                    Right r -> r

getAllPaperTitles :: FilePath -> IO [(Int, T.Text)]
getAllPaperTitles fp = do
    lns <- T.lines <$> TIO.readFile fp
    return $ map parseTitle' lns

getCitationFromFile :: FilePath -> IO [(Int, T.Text)]
getCitationFromFile fp = do
    tit <- getAllPaperTitles fp
    forConcurrently tit $ \(i, t) -> do
        cit <- getBibtexCitation t i
        return (i, cit)

convertRefsToBibtex :: FilePath -> FilePath -> IO ()
convertRefsToBibtex inName outName = do
    cits <- getCitationFromFile inName
    let allT = foldr (\(_, c)  acc -> c <> "\n" <> acc) "" cits
    TIO.writeFile outName allT

main :: IO ()
main = do
    [a, b] <- getArgs
    convertRefsToBibtex a b
