module Data.RandSet (
                     RandSet, empty, Data.RandSet.null, add,
                     findByWeight, randomChoice
                    ) where

    import System.Random

    data RandSet w v = Nil | Branch w v Int (RandSet w v) (RandSet w v)
                     deriving (Eq)

    empty :: RandSet w v
    empty = Nil

    null :: RandSet w v -> Bool
    null Nil = True
    null _   = False

    weight :: (Num w) => RandSet w v -> w
    weight Nil = 0
    weight (Branch w _ _ _ _) = w

    height :: RandSet w v -> Int
    height Nil = 0
    height (Branch _ _ h _ _) = h

    rotater :: (Num w) => RandSet w v -> RandSet w v
    rotater Nil = Nil
    rotater (Branch w v h Nil r) = Branch w v h Nil r
    rotater (Branch w v _ (Branch w' v' _ l' r') r ) = 
                    Branch neww' v' h' l' (Branch neww v h r' r)
        where h = max (height r') (height r) + 1
              h' = max (height l') h + 1
              neww = w + weight r' - w'
              neww' = w
                           
    rotatel :: (Num w) => RandSet w v -> RandSet w v
    rotatel Nil = Nil
    rotatel (Branch w v h l Nil) = Branch w v h l Nil
    rotatel (Branch w v _ l (Branch w' v' _ l' r')) = 
                    Branch neww' v' h' (Branch neww v h l l') r'
        where h = max (height l) (height l') + 1
              h' = max h (height r') + 1
              neww = w + weight l' - w'
              neww' = w

    balanceFactor :: RandSet w v -> Int
    balanceFactor Nil = 0
    balanceFactor (Branch _ _ _ l r) = height l - height r

    balance :: (Num w) => RandSet w v -> RandSet w v
    balance Nil = Nil
    balance b@(Branch w v h l r) 
        | bf == 2 && bfl == -1 = rotater (Branch w v h (rotatel l) r)
        | bf == 2 && bfl /= -1 = rotater (Branch w v h l r)
        | bf == -2 && bfr == 1 = rotatel (Branch w v h l (rotater r))
        | bf == -2 && bfr /= 1 = rotatel (Branch w v h l r)
        | otherwise = b
        where bf = balanceFactor b 
              bfl = balanceFactor l
              bfr = balanceFactor r
              
    add :: (Num w, Ord v) => (v,w) -> RandSet w v -> RandSet w v
    add (v,w) Nil = Branch w v 1 Nil Nil
    add (v,w) (Branch w' v' h' r l)
        | v == v'    = Branch (w+w') v' h' r l
        | v < v'     = balance $ Branch (w+w') v' newhr r' l
        | otherwise  = balance $ Branch (w+w') v' newhl r l'
        where l' = add (v,w) l
              r' = add (v,w) r
              newhr = 1 + max (height r') (height l)
              newhl = 1 + max (height r) (height l)

    findByWeight :: (Ord w, Num w) => w -> RandSet w v -> v
    findByWeight _ Nil = error "Cannot choose an element from an empty RandSet"
    findByWeight _ (Branch _ v _ Nil Nil) = v
    findByWeight w' (Branch w v _ l r)
        | w' <= weight l = findByWeight w' l
        | w' > w - weight r = findByWeight (w' - w + weight r) r
        | otherwise = v

    randomChoice :: (RandomGen g, Random w, Ord w, Num w) => 
                    g -> RandSet w v -> (g, v)
    randomChoice g rs = (g', findByWeight w rs)
        where (w,g') = randomR (1, weight rs) g
