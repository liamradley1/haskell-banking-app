{-|
Module :      Transactions
Description : Provides the framework necessary for carrying out financial transactions within the program.

Provides the framework necessary for carrying out financial transactions within the program.
-}

module Transactions(transfer, credit, debit) where

import Control.Concurrent
import CustomerData

-- | Increments a customer's balance by a given rational amount.
credit :: Customer -> Rational -> IO ()
credit cBefore amount = do 
    currentBal <- takeMVar (balance cBefore) 
    let newBal = currentBal + amount
    putMVar (balance cBefore) newBal

-- | Decrements a customer's balance by a given rational amount. 
debit :: Customer -> Rational -> IO ()
debit cBefore amount = do
    currentBal <- takeMVar (balance cBefore)
    let newBal = currentBal - amount
    putMVar (balance cBefore) newBal

-- | Automation function combining the credit and debit functions. The locks are handled by the credit and debit functions to ensure data consistency.
transfer :: Customer -> Customer -> Rational -> IO ()
transfer payFrom payTo amount = do
    debit payFrom amount
    credit payTo amount
    tempFrom <- readMVar (balance payFrom)
    tempTo <- readMVar (balance payTo)
    let tempA = fromRational tempFrom :: Double
    let tempB = fromRational tempTo :: Double
    putStrLn $ "New balance for " ++ name payFrom ++ ": " ++ show tempA
    putStrLn $ "New balance for " ++ name payTo ++ ": " ++ show tempB