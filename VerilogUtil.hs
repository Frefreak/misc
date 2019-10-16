{-# LANGUAGE OverloadedStrings #-}
module Util where

import Data.Attoparsec.Text
import Data.List
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO
import Control.Monad
import Text.Regex.PCRE
import Data.Monoid

extractModuleLine' :: BS.ByteString -> BS.ByteString -> BS.ByteString
extractModuleLine' fileContent modName =
    let pat = "module\\s+" <> modName <> "\\s*\\(.*\\)\\s*;"
        result = getAllTextMatches $ fileContent =~ pat :: [BS.ByteString]
    in if null result then "" else head result

extractModuleLine :: BS.ByteString -> BS.ByteString
extractModuleLine fileContent =
    let pat = "module\\s+" <> ".+" <> "\\s*\\([^)]*\\)\\s*;" :: BS.ByteString
        result = getAllTextMatches $ fileContent =~ pat :: [BS.ByteString]
    in if null result then "" else head result

parseModule :: T.Text -> T.Text
parseModule = uncurry formatInstance . analyzeModule

analyzeModule :: T.Text -> (T.Text, [T.Text])
analyzeModule modLine =
    let result = parseOnly parser modLine
        parser :: Parser (T.Text, [T.Text])
        parser = do
            string "module" >> skipSpace
            modN <- takeWhile1 (notInClass "\t\n (")
            void $ skipSpace >> char '('
            modIOs <- (takeWhile1 (notInClass ",)"))
                            `sepBy` char ','
            void $ char ')' >> skipSpace >> char ';'
            return (modN, map (last . T.words) modIOs)
    in case result of
            Left err -> error $ "Parse fail: " <> err
            Right r -> r

formatInstance :: T.Text -> [T.Text] -> T.Text
formatInstance modName ios =
    f <> body <> l where
        f = modName <> " U0 (\n"
        l = ");"
        body = T.intercalate ",\n" (toIO ios) <> "\n"
        toIO ts = let   maxL = maximum . map T.length $ ts
                    in  map (toIO' maxL) ts
        toIO' n t = let n' = n - T.length t + 2
                    in "." <> t <> T.replicate n' " " <> "(" <> t <> ")"

generateModuleInstance :: FilePath -> IO T.Text
generateModuleInstance fp = do
    content <- BS.readFile fp
    let moduleLine = decodeUtf8 $ extractModuleLine content
    return $ parseModule moduleLine

-- generate module instance code block
gmi :: FilePath -> IO ()
gmi = generateModuleInstance >=> TIO.putStrLn

-- generate single module testbench file template
gmtb :: FilePath -> IO ()
gmtb fp = do
    content <- BS.readFile fp
    let moduleLine = decodeUtf8 $ extractModuleLine content
        (modName, _) = analyzeModule moduleLine
        ib = parseModule moduleLine
        tb = T.unlines ["module " <> modName <> "_tb;\n",
            ib, "\ninitial\nbegin\n\nend\n\nendmodule"]
    TIO.appendFile (T.unpack modName <> "_tb.v") tb

data TruthTable = TruthTable {
    ins :: [String],
    outs :: [String]
}

getTruthTable :: String -> TruthTable
getTruthTable tw =
    let (a, b) = span (not . all (== ' ')) $ transpose (lines tw)
     in TruthTable (transpose a) (tail b)

generateVerilog' :: TruthTable -> String -> String -> [String]
generateVerilog' (TruthTable ins outs) inName outName =
    let m = map (\n -> [a | (a, b) <- zip ins n, b == '1']) outs
        pV :: [String] -> String
        pV s = intercalate "|" (map pV' s)
        pV' :: String -> String
        pV' s = let l = length s
                    vars = map (\b -> "(" ++ inName ++ "[" ++ show b ++ "])") $ (reverse [0..l - 1])
                in "(" ++ (intercalate "&" $ map (\(a, b) -> h a b) $ zip s vars) ++ ")"
        h a b = if a == '1' then b else "~" ++ b :: String
        rights = map pV m
        lefts = map (\s -> outName ++ show s) (reverse [0..length outs - 1])
    in map (\(a, b) -> "assign " ++ a ++ " = " ++ b ++ ";") $ zip lefts rights

-- output verilog code from truth table file
-- file looks like this
-- 00 1
-- 01 1
-- 10 0
-- 11 0
generateVerilog :: FilePath -> String -> String -> IO ()
generateVerilog fp inName outName = do
    t <- Prelude.readFile fp
    let stmts = generateVerilog' (getTruthTable t) inName outName
    mapM_ Prelude.putStrLn stmts

