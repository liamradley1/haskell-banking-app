{-|
Module :      Handler
Description : A module housing the handler function, which deals with the process each thread will need to use to interact with the program successfully.

A module housing the handler function, which deals with the process each thread will need to use to interact with the program successfully.
-}

module Handler (handler) where

import CustomerData
import Transactions
import Randomisers
import Control.Concurrent

-- | Workhorse handler for individual threads.
handler :: MVar Int -> Customer -> [Customer] -> MVar String -> IO ()
handler count threadOwner customers complete = do
    counter <- readMVar count -- Reads the count to perform an evaluation as to whether the  
    if counter < 101 then do -- Performs initial check to deflect any threads that attempt to run after the 100th attempt.
        customerTo <- randomCustomer customers
        if threadOwner == customerTo then do -- The thread owner is trying to transfer to themselves. This is not allowed.
            putStrLn $ "\nAccount holder " ++ name threadOwner ++ " is attempting to transfer money to themselves. Rerolling the recipient."
            handler count threadOwner customers complete
        else do
            amount <- randomAmount threadOwner
            if amount > 0 then do -- The thread owner has enough money to make a transaction, so does so.
                c <- takeMVar count
                if c < 101 then do -- Final check to catch threads that missed the initial check due to running in parallel.
                    putStrLn $ "\nCount: " ++ show c
                    putStrLn $ "Account holder " ++ name threadOwner ++ " transferred £" ++ show (fromRational amount) ++ " to account holder " ++ name customerTo ++ "."
                    transfer threadOwner customerTo amount
                    putMVar count (c + 1)
                    delay <- randomTime
                    threadDelay delay
                    handler count threadOwner customers complete
                else do 
                    putMVar count c 
                    handler count threadOwner customers complete
            else do -- The thread owner does not have enough money to make a transaction, so their thread is delayed for double the maximum normal delay so the others can continue.
                putStrLn $ "\n Account holder " ++ name threadOwner ++ " does not have enough money to make a transfer. Delaying thread to give others a chance to transfer."
                threadDelay 400000
                handler count threadOwner customers complete
    else do -- There have been 100 iterations, so the program exits.
        putMVar complete ""
        c <- takeMVar complete 
        putStr ""
