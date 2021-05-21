-- test that sendAndBroadcast doesn't block

import System.Concurrency.BufferedChannel

main : IO ()
main = do bcRef <- makeBufferedChannel
          (MkDPair bc send) <- becomeSender Broadcast bcRef
          send bc 3
          putStrLn "Success."

