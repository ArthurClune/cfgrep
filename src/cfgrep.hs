{-# Language OverloadedStrings #-}

import           Control.Error.Util         (errLn)
import qualified Data.ByteString.Lazy as B
import           Data.Monoid                (mempty, (<>))
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy as T
import           Options.Applicative        ((<*>), (<$>), argument, arguments,
                                            execParser, info, metavar, short, str, switch,
                                            value)
import           System.FilePath.Find       ((==?), (||?), always, extension, find)
import           System.Exit                (ExitCode(..), exitWith)
import           System.IO                  (Handle, IOMode(..), stdin, openFile)
import           Text.HTML.DOM              (parseLBS)
import           Text.XML.Cursor            (fromDocument)
import           Text.XML.Scraping          (toHtml)
import           Text.XML.Selector          (query)

data CFGrep = CFGrep {
                     optRecurse :: Bool,
                     optPattern :: String,
                     optFiles   :: [String]
                     } deriving (Show)

findMatches::String -> B.ByteString -> Maybe Text
findMatches pat page = do
    let results = query pat $ fromDocument $ parseLBS page
    if null results
        then Nothing
        else Just $ toHtml results

matchOverHandle::String->Handle->IO (Maybe Text)
matchOverHandle pat fh = do
    contents <- B.hGetContents fh
    return $! findMatches pat contents

matchOverFiles::String->[String]->IO [Maybe Text]
matchOverFiles pat = mapM (\f -> openFile f ReadMode >>= matchOverHandle pat)

printWithFilename::(String, Maybe Text) -> IO ()
printWithFilename (name, Just results) = putStrLn $ "\n" ++ name ++ ": \n\n" ++ T.unpack results
printWithFilename (_, Nothing)         = return ()

printResult::Maybe Text -> IO ()
printResult (Just s) = print s
printResult Nothing  = return ()

main :: IO ()
main = do
    options <- execParser opts
    let pat = optPattern options
    if optRecurse options
        then case optFiles options of
            []  -> do
                        errLn "Must specify a directory!"
                        exitWith (ExitFailure 1)
            [f] -> do
                       files <- find always (extension ==? ".cfm" ||? extension ==? ".cfc") f
                       results <- matchOverFiles pat files
                       mapM_ printWithFilename (zip files results)
            _  -> do
                       errLn "Sorry, can only recurse down one directory at a time"
                       exitWith (ExitFailure 1)
        else case optFiles options of
            []  -> matchOverHandle pat stdin
                   >>= printResult
            [f] -> matchOverFiles pat [f]
                   >>= printResult . head
            l   -> matchOverFiles pat l
                   >>= \x -> mapM_ printWithFilename (zip (optFiles options) x)
  where
    parser = CFGrep <$> switch (short 'r' <> value False)
                    <*> argument  str (metavar "pattern")
                    <*> arguments str (metavar "filenames")
    opts = info parser mempty
