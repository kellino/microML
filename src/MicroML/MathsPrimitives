module MicroML.MathsPrimitives where

import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, randomR)

type R a = State StdGen a

type Seed = Int

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

randInt :: Int -> Int -> R Int
randInt lo hi = do
    gen <- get
    let (r, gen') = randomR (lo, hi) gen
    put gen'
    return r

randDouble :: R Double
randDouble = do
    gen <- get
    let (r, gen') = randomR (1, 10) gen
    put gen'
    return r

{-ints :: Int -> Int -> R [Int]-}
{-ints lo hi = mapM (randInt lo hi) $ repeat ()-}

{-randInts :: Int -> R [Int]-}
{-randInts x = fmap (take x) ints-}

doubles :: R [Double]
doubles = mapM (const randDouble) $ repeat ()

randDoubles :: Int -> R [Double]
randDoubles x = fmap (take x) doubles

test :: R [Double]
test = randDoubles 5 

_runD :: Int -> [Double]
_runD = runRandom test 
