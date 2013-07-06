{-# Language OverloadedStrings #-}

import Control.Monad              ((>=>))
import Data.Monoid                (mempty)
import Data.List.Split            (split, keepDelimsL, onSublist)
import Options.Applicative        ((<*>), (<$>), argument, arguments,
                                   execParser, info, metavar, str)
import System.IO                  (Handle, IOMode(..), hGetContents, stdin, openFile)
import Text.HandsomeSoup          (css)
import Text.XML.HXT.Core hiding   (trace)
import Text.XML.HXT.TagSoup

data CFGrep = CFGrep {
                     pattern :: String,
                     files :: [String]
                     } deriving (Show)

findMatches::String -> String -> IO [String]
findMatches pat page = do
    results <- runX . xshow $
                    readString [
                        withParseHTML yes,
                        withTagSoup,
                        withValidate no,
                        withWarnings no,
                        withPreserveComment yes,
                        withCanonicalize no,
                        -- withStrictInput yes,
                        withEncodingErrors yes
                    ]
                    page >>> css pat
    return $ split (keepDelimsL $ onSublist ('<' : pat)) (head results)

matchOverHandle::String->Handle->IO [String]
matchOverHandle pat = hGetContents >=> findMatches pat

printWithFilename::(String, [String]) -> IO ()
printWithFilename (name, results) = mapM_ (putStrLn . (\x -> name ++ ": " ++ x)) results

main :: IO ()
main = do
    options <- execParser opts
    case files options of
         []  -> matchOverHandle (pattern options) stdin
                >>= mapM_ putStrLn
         [f] -> openFile f ReadMode
                >>= matchOverHandle (pattern options)
                >>= mapM_ putStrLn
         l   -> mapM (\f -> openFile f ReadMode >>= matchOverHandle (pattern options)) l
                >>= \x -> mapM_ printWithFilename (zip (files options) x)
  where
    parser = CFGrep <$> argument  str (metavar "pattern")
                    <*> arguments str (metavar "filenames")
    opts = info parser mempty
