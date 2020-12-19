{-|
Module :      Randomisers
Description : Provides a set of random entity generators for use within the program. These consist of creation of random amounts and the selection of random customers.

Provides a set of random entity generators for use within the program. These consist of creation of random amounts and the selection of random customers.
-}

module Randomisers(randomCustomer, randomAmount, randomTime) where

import Control.Concurrent
import CustomerData
import System.Random
import Data.Ratio.Rounding

-- | Returns a random rational number determined by the balance of the customer in question. Checks are performed to ensure the customer has enough money to perform a transaction, and if so will only perform a transaction that they can afford.
randomAmount :: Customer -> IO Rational
randomAmount customer = do
    bal <- readMVar (balance customer)
    if bal < 10 then return $ toRational 0
    else do
        let amount = randomRIO (10 :: Double, fromRational bal :: Double)
        random <- (amount :: IO Double)
        let randomValue = dpRound 2 (toRational random) 
        if bal < randomValue then 
            randomAmount customer
        else return randomValue

-- | Returns a random customer from the a list of 10 customers taken as input.
randomCustomer :: [Customer] -> IO Customer
randomCustomer customers = do
    x <- (randomRIO (0, 1) :: IO Double)
    if x < 0.1 then do
        return $ head customers
    else if x < 0.2 then do
        return $ customers !! 1
    else if x < 0.3 then do
        return $ customers !! 2
    else if x < 0.4 then do 
        return $ customers !! 3
    else if x < 0.5 then do
        return $ customers !! 4
    else if x < 0.6 then do
        return $ customers !! 5
    else if x < 0.7 then do
        return $ customers !! 6
    else if x < 0.8 then do
        return $ customers !! 7
    else if x < 0.9 then do
        return $ customers !! 8
    else do
        return $ last customers


-- | Returns a random number corresponding to a time delay between 0.1s and 0.2s.
randomTime :: IO Int
randomTime = randomRIO (100000, 200000)