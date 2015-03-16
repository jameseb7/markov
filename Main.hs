import Control.Monad

import Data.List
import qualified Data.MarkovChain as MarkovChain
import Data.MarkovChain (MarkovChain, emptyNGram)

import System.Console.GetOpt
import System.Environment
import System.IO.Strict
import System.Random

import Prelude hiding (readFile)

data Options = Options {
      optWords :: Int,
      optNGramSize :: Int
    }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option "w" ["words"] 
                 (ReqArg (\ n opts -> opts{optWords=read n}) "INT")
                 "number of words to output" 
    , Option "n" ["ngram-size"] 
                 (OptArg (\ n opts -> opts{optNGramSize = maybe 2 read n}) "INT")
                 "length of the prefix that the next word depends on" ]

defaultOpts :: Options
defaultOpts = Options {
                optWords = 0,
                optNGramSize = 2
              }

parseOpts :: IO (Options, [FilePath])
parseOpts = do args <- getArgs
               case getOpt Permute options args of
                 (opts, files, []) -> 
                     return (foldl (flip ($)) defaultOpts opts, files)
                 (_, _, errs)      -> 
                     ioError . userError $ unlines errs ++ usageInfo "" options

addLine :: Int -> MarkovChain String -> [String] -> MarkovChain String
addLine n m = snd . foldl' MarkovChain.addElement (emptyNGram n, m)

main :: IO ()
main = do (opts, files) <- parseOpts
          input <- liftM unlines $ mapM readFile files
          let n = optNGramSize opts
          let input' = map words $ lines input
          let mc = foldl' (addLine n) MarkovChain.empty input'
          g <- newStdGen
          let text = unfoldr (MarkovChain.nextElement mc) (emptyNGram n, g)
          putStrLn . unwords $ take (optWords opts) text
