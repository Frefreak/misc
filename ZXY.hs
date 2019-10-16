{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module ZXY where

import           ClassyPrelude
import           Network.HTTP.Req
import           Network.HTTP.Client hiding (responseBody)
import qualified Network.HTTP.Client as NHC (responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status
import           Control.Lens.Operators
import           Text.HTML.DOM hiding (readFile)
import           Text.XML.Cursor
import           Data.Aeson.Lens
import           Control.Lens (to)
import           Control.Monad (forM)
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)
import           Data.Maybe (fromJust)
import           Network.HTTP.Types.Header (hLocation)
import           Text.Regex.PCRE.Heavy
import           Data.Default
import qualified Prelude as Pre
import qualified Data.ByteString.Char8 as BS
import           System.Process
import           System.IO (hPutStr)

initZXY :: IO ()
initZXY = do
    putStrLn "Setup:"
    zxyRoot >>= createDirectoryIfMissing True
    initBaiduPanCookie
    initNeu6Cookie

zxyRoot :: IO FilePath
zxyRoot = (\d -> d </> ".cache" </> "ZXY") <$> getHomeDirectory

getCookieContent :: FilePath -> IO ByteString
getCookieContent fn = do
    cookie <- readFile fn
    if BS.last cookie == '\n' then return (BS.init cookie) else return cookie

-- req releted {{{

https' :: ByteString -> (Url 'Https, Option scheme)
https' = fromJust . parseUrlHttps

http' :: ByteString -> (Url 'Http, Option scheme)
http' = fromJust . parseUrlHttp

instance MonadHttp IO where
    handleHttpException (VanillaHttpException e) = throwIO e
    handleHttpException e = throwIO e

type IOReader = ReaderT HttpConfig IO

instance MonadHttp IOReader where
    handleHttpException (VanillaHttpException e) = throwIO e
    handleHttpException e = throwIO e
    getHttpConfig = ask
-- }}}

-- btso utility {{{

btsoUrl :: Url 'Https
btsoUrl = https "btso.pw"

btSearchUrl :: Url 'Https
btSearchUrl = btsoUrl /: "search"

btsoOpt :: Option scheme
btsoOpt = header "Accept-Language" "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4,en-GB;q=0.2"
       <> header "Upgrade-Insecure-Requests" "1"
       <> header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36"
       <> header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
       <> header "Accept-Encoding" "gzip, deflate, sdch, br"

mkKeyword :: Text -> Option scheme
mkKeyword sn = btsoOpt <> "keyword" =: sn

getLinkUrls :: Text -> IO [Text]
getLinkUrls sn = do
    r <- req GET btSearchUrl NoReqBody lbsResponse (mkKeyword sn)
    return $ parseLinks (responseBody r)

parseLinks :: LByteString -> [Text]
parseLinks lbs =
   let cursor = fromDocument $ parseLBS lbs
       links = cursor $// element "div" >=> attributeIs "class" "data-list"
               &// element "a" >=> attribute "href"
   in links

getMagnetUrl :: Text -> IO (Maybe Text)
getMagnetUrl url = do
   r <- req GET (fst . https' $ encodeUtf8 url) NoReqBody lbsResponse btsoOpt
   return $ parseMagnet $ responseBody r

parseMagnet :: LByteString -> Maybe Text
parseMagnet lbs =
   let cursor = fromDocument $ parseLBS lbs
       magnet = cursor $// element "textarea" >=> attributeIs "id" "magnetLink"
               &// content
   in headMay magnet

getMagnets :: Text -> IO [Text]
getMagnets sn = do
   ts <- take 20 <$> getLinkUrls sn
   {- mags <- mapConcurrently getMagnetUrl ts -}
   mags <- mapM getMagnetUrl ts
   return $ catMaybes mags

getMagnets' :: Text -> IO ()
getMagnets' = getMagnets >=> mapM_ putStrLn

-- }}}

-- baidu pan utility {{{

baiduPanCacheFile :: IO FilePath
baiduPanCacheFile = (</> "baidu_netdisk") <$> zxyRoot

initBaiduPanCookie :: IO ()
initBaiduPanCookie = do
    fn <- baiduPanCacheFile
    putStrLn $ "Copy baidu netdisk cookie to " <> pack fn

baiduPanOpts :: IO (Option scheme)
baiduPanOpts = do
    fn <- baiduPanCacheFile
    cookie <- getCookieContent fn
    return $ header "Origin" "https://pan.baidu.com"
          <> header "X-Requested-With" "XMLHttpRequest"
          <> header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.81 Safari/537.36"
          <> header "Accept" "application/json, text/javascript, */*; q=0.01"
          <> header "Accept-Encoding" "gzip, deflate, br"
          <> header "Referer" "https://pan.baidu.com/disk/home"
          <> header "Cookie" cookie

baiduPanApiUrl :: Int -> (Url 'Https, Option scheme)
baiduPanApiUrl ct =
    https' $ "https://pan.baidu.com/rest/2.0/services/cloud_dl?channel=chunlei&web=1&app_id=250528&clienttype=" <> BS.pack (show ct)

mkBasicPostBody :: Text -> Text -> FormUrlEncodedParam
mkBasicPostBody magnet pa = ("source_url" =: magnet) <> ("save_path" =: pa)

mkQueryBody :: Text -> Text -> FormUrlEncodedParam
mkQueryBody magnet pa = ("method" =: ("query_magnetinfo" :: Text))
    <> mkBasicPostBody magnet pa

mkAddTaskBody :: Text -> Text -> [Int] -> FormUrlEncodedParam
mkAddTaskBody magnet pa sel =
    let selt = intercalate "," $ map tshow sel
        basic = mkBasicPostBody magnet pa
    in ("selected_idx" =: selt) <> ("method" =: ("add_task" :: Text)) <> basic

testMagnet :: Text
testMagnet = "magnet:?xt=urn:btih:C2926560EFEA1EE145176A65DBA34E0A22A6993F"

queryMagnetInfo :: Text -> IO [(Int, Text)]
queryMagnetInfo mag = do
    let dat = ReqBodyUrlEnc $ mkQueryBody mag ""
        handler e@(HttpExceptionRequest _ (StatusCodeException r _)) =
            if responseStatus r == status403 then return "" else throwIO e
        handler e = throwIO e
        (url, opt) = baiduPanApiUrl 1
    bpOpt <- baiduPanOpts
    r <- (responseBody <$> req POST url dat lbsResponse (opt <> bpOpt))
            `catch` handler
    let tups = r ^.. key "magnet_info" . _Array . traverse .
            to (\o -> (o ^. key "size" . _String, o ^. key "file_name" . _String))
    return $ map (first $ fromMaybe 0 . readMay) tups

addTask :: Text -> Text -> [Int] -> IO Bool
addTask mag pa sels = do
    let dat = ReqBodyUrlEnc $ mkAddTaskBody mag pa sels
        (url, opt) = baiduPanApiUrl 0
    bpOpt <- baiduPanOpts
    r <- try $ req POST url dat lbsResponse (opt <> bpOpt)
    case r of
        Right rsp -> do
            let m = responseBody rsp ^? key "rapid_download" . _Number
                rapid_download = fromMaybe 0 m
            return $ rapid_download == 1
        Left e@(HttpExceptionRequest _ (StatusCodeException resp _)) ->
            if responseStatus resp == status403
              then do
                let msg = fromMaybe ""
                        (lookup "X-Response-Body-Start" $ responseHeaders resp)
                    errorMsg = msg ^. key "error_msg" . _String
                if errorMsg == "vcode is needed"
                  then putStrLn "\ESC[31;1mVcode is up!\ESC[0m" >> throwIO e
                  else return False
              else throwIO e
        Left e -> throwIO e

-- debugging purpose
addVideoTaskDebug :: Text -> Text -> IO LbsResponse
addVideoTaskDebug mag p = do
    tups <- queryMagnetInfo mag
    let sel = filterVideoFile tups
        dat = ReqBodyUrlEnc $ mkAddTaskBody mag p sel
        (url, opt) = baiduPanApiUrl 0
    bpOpt <- baiduPanOpts
    req POST url dat lbsResponse (opt <> bpOpt)

filterVideoFile :: [(Int, Text)] -> [Int]
filterVideoFile tups =
    let idxTup = zip [1..] tups
        minVideoSize = 200 * 1024 * 1024 -- 200M
        checkTup (s, _) = s > minVideoSize
    in filter (\(_, tup) -> checkTup tup) idxTup & map fst

addVideoTask :: Text -> IO Bool
addVideoTask keyword = do
    mags <- getMagnets keyword
    let n = tshow $ length mags
    putStrLn $ "total: " <> n
    bools <- forM (zip [(1 :: Int)..] mags) $ \(i, m) -> do
        putStr $ m <> " (\ESC[36;1m"<> tshow i <> "/" <> n <> "\ESC[0m) "
        tups <- queryMagnetInfo m
        let sel = filterVideoFile tups
        if not (null sel)
          then do
            b <- addTask m ("/auto/" <> filter (/= ' ') keyword) sel
            if b then putStrLn "\ESC[32;1mSuccess\ESC[0m"
                 else putStrLn "\ESC[31;1mFail\ESC[0m"
            return b
          else putStrLn "\ESC[34;1mEmpty\ESC[0m" >> return False
    return $ or bools

-- add task while reporting status
addVideoTask' :: Text -> IO ()
addVideoTask' keyword = do
    b <- addVideoTask keyword
    if b then putStrLn "\ESC[32;1mSuccess\ESC[0m"
         else putStrLn "\ESC[31;1mFail\ESC[0m"

-- access share page
{- accessSharePage :: Text -> IO () -}
accessSharePage url = do
    cookie <- baiduPanCacheFile >>= getCookieContent
    let headers = [("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.81 Safari/537.36"),
                    ("Cookie", cookie)]
    r <- parseRequest url
    let newr = r {requestHeaders = headers}
    manager <- newManager tlsManagerSettings
    hist <- responseOpenHistory newr manager
    let location = queryString (hrFinalRequest hist)
    BS.putStrLn $ location
    NHC.responseBody $ hrFinalResponse hist
-- }}}

-- neu6 utility {{{

neu6CacheFile :: IO FilePath
neu6CacheFile = (</> "neu6") <$> zxyRoot

initNeu6Cookie :: IO ()
initNeu6Cookie = do
    fn <- neu6CacheFile
    putStrLn $ "Copy neu6 forum cookie to " <> pack fn

neu6Opt :: IO (Option scheme)
neu6Opt = do
    fn <-neu6CacheFile
    cookie <- getCookieContent fn
    return $ header "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36"
          <> header "Upgrade-Insecure-Requests" "1"
          <> header "Host" "bt.neu6.edu.cn"
          <> header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
          <> header "Accept-Encoding" "gzip, deflate, sdch"
          <> header "Accept-Language" "en-US,en;q=0.8,zh-CN;q=0.6,zh;q=0.4,en-GB;q=0.2"
          <> header "Referer" "http://bt.neu6.edu.cn/thread-1571574-1-1.html"
          <> header "Cookie" cookie

neu6Url :: Url 'Http
neu6Url = http "bt.neu6.edu.cn"

mkNeu6Thread :: Text -> Url 'Http
mkNeu6Thread s = neu6Url /: s

-- assume torrent file is in the first floor, it's also possible to get
-- subtitles, all store in fp
downloadTorrent :: Text -> FilePath -> IO ()
downloadTorrent th fp = do
    let url = mkNeu6Thread th
    n6opt <- neu6Opt
    r <- req GET url NoReqBody lbsResponse n6opt
    let lbs = responseBody r
        cursor = fromDocument $ parseLBS lbs
        alinks = cursor $// element "p" >=> attributeIs "class" "attnm" &/
                element "a"
        linksAndNames = flip map alinks $
            \c -> (Pre.head $ c & attribute "href", Pre.head $ c $/ content)
        cfg = def { httpConfigRedirectCount = 0 }
    forM_ linksAndNames $ \(l', n) -> do
        let handler (HttpExceptionRequest _ (StatusCodeException resp _)) =
                return . fromStrict . fromJust $ Pre.lookup hLocation (responseHeaders resp)
            handler e = throwIO e
            l = gsub [re|&amp;|] ("&" :: Text) l'
        r' <- (responseBody <$> -- we expect 302 fails here
            runReaderT (req GET (mkNeu6Thread l) NoReqBody lbsResponse n6opt) cfg) `catch` handler
            {- (getWith (neu6Opt & redirects .~ 0) (mkNeu6Thread' l))) `catch` handler -}
        linkLBS <- req GET (mkNeu6Thread . toStrict $ decodeUtf8 r') NoReqBody lbsResponse n6opt
            {- getWith neu6Opt (mkNeu6Thread' . toStrict $ decodeUtf8 r') -}
        -- can extract the final url now
        let cursor2 = fromDocument (parseLBS $ responseBody linkLBS)
            downloadLink = cursor2 $// element "div" >=>
                attributeIs "id" "messagetext" &/ element "p" &/ element "a" >=>
                attribute "href"
        unless (null downloadLink) $ do
            realR <- req GET (mkNeu6Thread $ Pre.head downloadLink) NoReqBody lbsResponse n6opt
            {- realR <- getWith neu6Opt $ mkNeu6Thread' $ Pre.head downloadLink -}
            writeFile (fp </> unpack n) (toStrict $ responseBody realR)

downloadTorrents :: FilePath -> FilePath -> IO ()
downloadTorrents list fp = do
   createDirectoryIfMissing True fp
   lns <- BS.lines <$> readFile list
   forM_ lns $ \th -> downloadTorrent (decodeUtf8 th) fp

-- }}}

-- general utility {{{
paste :: String -> IO ()
paste s = do
    (v, _, _, p) <- createProcess (shell "xclip -selection clipboard")
                                  { std_in = CreatePipe }
    case v of
      Just h -> hPutStr h s >> hClose h >> waitForProcess p >> return ()
      Nothing -> Pre.putStrLn "failed"
-- }}}

---- vim: foldmethod=marker
