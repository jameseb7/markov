import Control.Monad

import Data.List
import qualified Data.MarkovChain as MarkovChain
import Data.MarkovChain (MarkovChain, TwoGram(TwoGram))

import System.Console.GetOpt
import System.Environment
import System.IO.Strict
import System.Random

import Prelude hiding (readFile)

data Options = Options {
      optWords :: Int
    }

options :: [OptDescr (Options -> Options)]
options = 
    [ Option "wn" ["words"] 
                 (ReqArg (\ n opts -> opts{optWords=read n}) "INT")
                 "number of words to output" ]

defaultOpts :: Options
defaultOpts = Options {
                optWords = 0
              }

parseOpts :: IO (Options, [FilePath])
parseOpts = do args <- getArgs
               case getOpt Permute options args of
                 (opts, files, []) -> 
                     return (foldl (flip ($)) defaultOpts opts, files)
                 (_, _, errs)      -> 
                     ioError . userError $ unlines errs ++ usageInfo "" options

addLine :: MarkovChain TwoGram String -> [String] -> MarkovChain TwoGram String
addLine m = snd . foldl' MarkovChain.addElement (TwoGram ("",""), m)

main :: IO ()
main = do (opts, files) <- parseOpts
          input <- liftM unlines $ mapM readFile files
          let input' = map words $ lines input
          let mc = foldl' addLine MarkovChain.empty input'
          g <- newStdGen
          let text = unfoldr (MarkovChain.nextElement mc) (TwoGram ("",""), g)
          putStrLn . unwords $ take (optWords opts) text
