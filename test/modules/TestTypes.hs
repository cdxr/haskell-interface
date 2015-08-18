{-# LANGUAGE PatternSynonyms #-}

module TestTypes where


a_0 :: Int -> (a -> Double) -> IO ()
a_1 :: Int -> (a -> Int)    -> IO ()

a_0 = undefined
a_1 = undefined


b_0 :: IO (IO Double)     -> (a -> Double) -> IO ()
b_1 :: IO (Double -> Int) -> (a -> Double) -> IO ()

b_0 = undefined
b_1 = undefined


c_0 :: (a -> b) -> (b -> c) -> (a -> c)
c_1 :: (b -> c) -> (a -> b) -> (a -> c)

c_0 = undefined
c_1 = undefined
