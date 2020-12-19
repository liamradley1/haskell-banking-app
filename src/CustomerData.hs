{-|
Module :      CustomerData
Description : Provides the necessary data type to represent a customer in the transactions.

Provides the necessary data type to represent a customer in the transactions.
-}

module CustomerData(Customer(Customer, name, accountNumber, balance), printInfo, printFinalCustomers) where

import Control.Concurrent

-- | The data type used for handling a customers. Treats balance as an MVar in order to avoid the computational expense of creating new customers every time an update is made to balance. Also allows a lock to be enforced on the customer being transferred to or from, so only one transaction can happen on an account at a time.
data Customer = Customer {
    name :: String,
    accountNumber :: String,
    balance :: MVar Rational
}   deriving (
        Eq -- ^ Use the standard derivation of Eq. 
    )
-- | Provides a pretty-printed format of the information stored in Customer. An instance of Show could not be created as extraction form an MVar requires an IO action, so will not work with Show.
printInfo :: Customer -> IO () 
printInfo (Customer name accountNumber balance) = do
    bal <- readMVar balance
    putStrLn $ "\n" ++ "Name: " ++ name ++ "\n" ++ "Account number: " ++ accountNumber ++ "\n" ++ "Balance: " ++ show (fromRational bal :: Double)

-- | Automation function to print the details of all customers.
printFinalCustomers :: [Customer] -> IO ()
printFinalCustomers [] = putStrLn ""
printFinalCustomers (x:xs) = do
    printInfo x
    printFinalCustomers xs