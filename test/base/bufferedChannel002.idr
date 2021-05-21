-- test that sendAndSignal doesn't block

import System.Concurrency.BufferedChannel

main : IO ()
main = do bcRef <- makeBufferedChannel
          (MkDPair bc send) <- becomeSender Signal bcRef
          send bc 3
          putStrLn "Success."

