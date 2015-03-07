module Data.RandSet where

    data RandSet w v = Nil | Branch w v Int (RandSet w v) (RandSet w v)
                     deriving (Show, Eq)

    empty :: RandSet w v
    empty = Nil

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
