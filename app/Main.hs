module Main where
import Control.Concurrent
import Control.Concurrent.ParallelIO.Global
import CustomerData
import System.Environment
import Handler

main :: IO ()
main = do
    balA <- newMVar 1000
    balB <- newMVar 1000
    balC <- newMVar 1000
    balD <- newMVar 1000
    balE <- newMVar 1000
    balF <- newMVar 1000
    balG <- newMVar 1000
    balH <- newMVar 1000
    balI <- newMVar 1000
    balJ <- newMVar 1000
    complete <- newEmptyMVar
    counter <- newMVar 1

    let cA = Customer "A" "1" balA 
    let cB = Customer "B" "2" balB 
    let cC = Customer "C" "3" balC 
    let cD = Customer "D" "4" balD
    let cE = Customer "E" "5" balE
    let cF = Customer "F" "6" balF
    let cG = Customer "G" "7" balG
    let cH = Customer "H" "8" balH
    let cI = Customer "I" "9" balI
    let cJ = Customer "J" "10" balJ
    let customers = [cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ]
    
    args <- getArgs
    case args of 
        ["concurrent"] -> do
            forkIO $ handler counter cA customers complete
            forkIO $ handler counter cB customers complete 
            forkIO $ handler counter cC customers complete 
            forkIO $ handler counter cD customers complete 
            forkIO $ handler counter cE customers complete 
            forkIO $ handler counter cF customers complete 
            forkIO $ handler counter cG customers complete
            forkIO $ handler counter cH customers complete 
            forkIO $ handler counter cI customers complete 
            forkIO $ handler counter cJ customers complete
            c <- takeMVar complete
            putStrLn "\nPrinting final customer information:"
            printFinalCustomers customers
            putStrLn "Exiting concurrent program."
    
        ["parallel"] -> do
            let threads = [ -- Define a list of IO () actions to be run in parallel.
                           handler counter cA customers complete,
                           handler counter cB customers complete,
                           handler counter cC customers complete,
                           handler counter cD customers complete, 
                           handler counter cE customers complete, 
                           handler counter cF customers complete, 
                           handler counter cG customers complete, 
                           handler counter cH customers complete, 
                           handler counter cI customers complete, 
                           handler counter cJ customers complete]
            parallel threads
            putStrLn "\nPrinting final customer information:"
            printFinalCustomers customers
            stopGlobalPool
            putStrLn "Exiting parallel program." 

        _ -> do
            putStrLn "\nUsage: System command [args]"
            putStrLn "concurrent     Runs the program using just concurrency."
            putStrLn "parallel       Runs the program using concurrency as well as parallelism"