{-# Language OverloadedStrings, TemplateHaskell #-}

import qualified Data.ByteString.Lazy as B
import           Data.Monoid                (mempty)
import           Data.Text.Lazy             (Text)
import           Options.Applicative        ((<*>), (<$>), argument, arguments,
                                            execParser, info, metavar, str)
import           System.IO                  (Handle, IOMode(..), stdin, openFile)

import           Text.HTML.DOM
import           Text.XML.Cursor            (fromDocument)
import           Text.XML.Scraping          (toHtml)
import           Text.XML.Selector


data CFGrep = CFGrep {
                     pattern :: String,
                     files :: [String]
                     } deriving (Show)

findMatches::String -> B.ByteString -> Maybe Text
findMatches pat page = do
    let doc = fromDocument $ parseLBS page
    let results = query pat doc
    if length results == 0
        then Nothing
        else Just (toHtml $ query pat doc)

matchOverHandle::String->Handle->IO (Maybe Text)
matchOverHandle pat fh = do
    contents <- B.hGetContents fh
    return $ findMatches pat contents

printWithFilename::(String, Maybe Text) -> IO ()
printWithFilename (name, Just results) = putStrLn $ name ++ ": " ++ (show results)
printWithFilename (_, Nothing)         = return ()

printResult::Maybe Text -> IO ()
printResult (Just s) = print s
printResult Nothing = return ()

main :: IO ()
main = do
    options <- execParser opts
    case files options of
         []  -> matchOverHandle (pattern options) stdin
                >>= printResult
         [f] -> openFile f ReadMode
                >>= matchOverHandle (pattern options)
                >>= printResult
         l   -> mapM (\f -> openFile f ReadMode >>= matchOverHandle (pattern options)) l
                >>= \x -> mapM_ printWithFilename (zip (files options) x)
  where
    parser = CFGrep <$> argument  str (metavar "pattern")
                    <*> arguments str (metavar "filenames")
    opts = info parser mempty
