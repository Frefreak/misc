{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Aeson (decode, Value(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Process (readProcess)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS


-- require `xclip`
getClipboard :: IO LBS.ByteString
getClipboard = LBS.pack <$> readProcess "xclip" ["-selection", "clipboard", "-o"] []

data CMD = CMD
    { filename :: Maybe FilePath
    }

parser :: Parser CMD
parser = CMD
    <$> optional (strOption (long "filename" <> short 'f' <> metavar "<FILE>"
                <> help "read json from file <FILE>"))

main :: IO ()
main = execParser (info (helper <*> parser) fullDesc) >>= exec

exec :: CMD -> IO ()
exec CMD{..} = do
    let prettify' = LBS.putStrLn . prettify
    case filename of
        Nothing -> getClipboard >>= prettify'
        Just f -> LBS.readFile f >>= prettify'

prettify :: LBS.ByteString -> LBS.ByteString
prettify t = let (val :: Maybe Value) = decode t
                in case val of
                    Nothing -> "invalid json"
                    Just val' -> encodePretty val'
