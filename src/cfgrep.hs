{-# Language OverloadedStrings #-}

import           Data.Monoid                (mempty)
import           Data.List.Split
import           Options.Applicative
import           Text.HandsomeSoup          (css)
import           Text.XML.HXT.Core hiding   (trace)
import           Text.XML.HXT.TagSoup

data CFGrep = CFGrep {
                     pattern  :: String,
                     filename :: String
                     } deriving (Show)


findMatches::String -> String -> IO [String]
findMatches pat page = do
    results <- runX . xshow $ readString [ withTagSoup,
                    withValidate no,
                    withWarnings no]
                    page >>> css pat
    return $ split (keepDelimsL $ onSublist ('<' : pat)) (head results)

main :: IO ()
main = do
    options <- execParser opts
    matches <- readFile (filename options) >>= findMatches (pattern options)
    mapM_ putStrLn matches
  where
    parser = CFGrep <$> argument str (metavar "pattern")
                    <*> argument str (metavar "filename")
    opts = info parser mempty
