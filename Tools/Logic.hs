module Tools.Logic where

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both p1 p2 val = and [p1 val, p2 val]