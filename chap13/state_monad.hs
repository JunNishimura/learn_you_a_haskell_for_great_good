-- import System.Random
-- import Control.Monad.State

-- threeCoins :: StdGen -> (Bool, Bool, Bool)
-- threeCoins gen = 
--     let (firstCoin, newGen) = random gen
--         (secondCoin, newGen') = random newGen
--         (thirdCoin, newGen'') = random newGen'
--     in (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x,xs)

-- push :: Int -> Stack -> ((), Stack)
-- push a xs = ((), a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack = let
--     ((), newStack1) = push 3 stack
--     (a, newStack2) = pop newStack1
--     in pop newStack2

-- newtype State s a = State { runState :: s -> (a, s) }
-- instance Monad (State s) where
--     return x = State $ \s -> (x, s)
--     (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                         (State g) = f a
--                                     in g newState
                                    
-- pop :: State Stack Int
-- pop = State $ \(x:xs) -> (x,xs)

-- push :: Int -> State Stack ()
-- push a = State $ \xs -> ((),a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     pop
--     pop

-- stackStuff :: State Stack ()
-- stackStuff = do
--     a <- pop
--     if a == 5
--         then push 5
--         else do
--             push 3
--             push 8

-- moreStack :: State Stack ()
-- moreStack = do
--     a <- stackManip
--     if a == 100
--         then stackStuff
--         else return ()

-- stackyStack :: State Stack ()
-- stackyStack = do
--     stackNow <- get
--     if stackNow == [1,2,3]
--         then put [8,3,1]
--         else put [9,2,1]

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- randomSt :: (RandomGen g, Random a) => State g a
-- randomSt = State random

-- threeCoins :: State StdGen (Bool, Bool, Bool)
-- threeCoins = do
--     a <- randomSt
--     b <- randomSt
--     c <- randomSt
--     return (a, b, c)