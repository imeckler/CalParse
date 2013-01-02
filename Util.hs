module Util where

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = fst . foldl1 maxOn . map (\x -> (x, f x))
    where maxOn (a, b) (c, d) = case compare d b of
                                    GT -> (c, d)
                                    _  -> (a, b)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = fst . foldl1 minOn . map (\x -> (x, f x))
    where minOn (a, b) (c, d) = case compare d b of
                                    LT -> (c, d)
                                    _  -> (a, b)
