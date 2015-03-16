module Data.MarkovChain where

    import Data.Default
    import Data.List
    import qualified Data.Map as Map
    import Data.Map(Map)
    import qualified Data.RandSet as RandSet
    import Data.RandSet(RandSet)

    import System.Random

    newtype NGram a = NGram [a] deriving (Eq, Ord, Show)

    shiftAppend :: NGram a -> a -> NGram a
    shiftAppend (NGram [])     y = NGram [y]
    shiftAppend (NGram (_:xs)) y = NGram (xs ++ [y])

    emptyNGram :: (Default a) => Int -> NGram a
    emptyNGram n = NGram (replicate n def)

    newtype MarkovChain a = MarkovChain (Map (NGram a) (RandSet Int a))

    empty :: MarkovChain a
    empty = MarkovChain Map.empty

    null :: MarkovChain a -> Bool
    null (MarkovChain m) = Map.null m

    addElement :: (Ord a) => 
                  (NGram a, MarkovChain a) -> a -> (NGram a, MarkovChain a)
    addElement (n, MarkovChain m) a = (n', MarkovChain m')
        where n' = shiftAppend n a
              m' = case Map.lookup n m of
                     Nothing -> Map.insert n (RandSet.add (a,1) RandSet.empty) m
                     Just r  -> Map.insert n (RandSet.add (a,1) r) m

    nextElement :: (RandomGen g, Ord a) => 
                   MarkovChain a -> (NGram a, g) -> Maybe (a, (NGram a, g))
    nextElement (MarkovChain m) (n,g) = fmap choose $ Map.lookup n m
        where choose r = (a, (shiftAppend n a, g'))
                  where (g',a) = RandSet.randomChoice g r

    construct1GramMC :: [String] -> MarkovChain String
    construct1GramMC = snd . foldl' addElement (emptyNGram 1, empty)

    generateFrom1GramMC :: (RandomGen g) => MarkovChain String -> g -> [String]
    generateFrom1GramMC m g = unfoldr (nextElement m) (emptyNGram 1, g)
