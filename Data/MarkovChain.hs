module Data.MarkovChain where

    import Data.List
    import qualified Data.Map as Map
    import Data.Map(Map)
    import qualified Data.RandSet as RandSet
    import Data.RandSet(RandSet)

    import System.Random

    class NGram n where
        shiftAppend :: n a -> a -> n a

    newtype OneGram a = OneGram a deriving (Eq, Ord, Show)
    instance NGram OneGram where
        shiftAppend (OneGram _) = OneGram

    newtype TwoGram a = TwoGram (a,a) deriving (Eq, Ord, Show)
    instance NGram TwoGram where
        shiftAppend (TwoGram (_,a)) b = TwoGram (a,b)

    newtype ThreeGram a = ThreeGram (a,a,a) deriving (Eq, Ord, Show)
    instance NGram ThreeGram where
        shiftAppend (ThreeGram (_,a,b)) c = ThreeGram (a,b,c)

    newtype MarkovChain n a = MarkovChain (Map (n a) (RandSet Int a))
    
    empty :: MarkovChain n a
    empty = MarkovChain Map.empty

    null :: MarkovChain n a -> Bool
    null (MarkovChain m) = Map.null m

    addElement :: (NGram n, Ord (n a), Ord a) =>
                  (n a, MarkovChain n a) -> a -> (n a, MarkovChain n a)
    addElement (n, MarkovChain m) a = (n', MarkovChain m')
        where n' = shiftAppend n a
              m' = case Map.lookup n m of
                     Nothing -> Map.insert n (RandSet.add (a,1) RandSet.empty) m
                     Just r  -> Map.insert n (RandSet.add (a,1) r) m

    nextElement :: (RandomGen g, NGram n, Ord (n a)) => 
                   MarkovChain n a -> (n a, g) -> Maybe (a, (n a, g))
    nextElement (MarkovChain m) (n,g) = fmap choose $ Map.lookup n m
        where choose r = (a, (shiftAppend n a, g'))
                  where (g',a) = RandSet.randomChoice g r

    construct1GramMC :: [String] -> MarkovChain OneGram String
    construct1GramMC = snd . foldl' addElement (OneGram "", empty)

    generateFrom1GramMC :: (RandomGen g) => 
                           MarkovChain OneGram String -> g -> [String]
    generateFrom1GramMC m g = unfoldr (nextElement m) (OneGram "", g)
