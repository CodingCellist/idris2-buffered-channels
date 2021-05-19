import System

import System.Concurrency.Pipe

{-- TODO: REWRITE EVERYTHING!
testMakeQueue : IO ()
testMakeQueue = do putStrLn "--- Begin testMakeQueue ---"
                   qRef <- makeQueue
                   q <- readIORef qRef
                   f <- readIORef $ front q
                   putStrLn "Front empty?"
                   if length f /= 0
                      then putStrLn "ERROR: Front not initially empty"
                      else do putStrLn "\tYes"
                              r <- readIORef $ rear q
                              putStrLn "Rear empty?"
                              if length r /= 0
                                 then putStrLn "ERROR: Rear not initially empty"
                                 else do putStrLn "\tYes"
                                         putStrLn "Can acquire mutex?"
                                         mutexAcquire $ lock q
                                         putStrLn "\tYes"
                                         putStrLn "Can release mutex?"
                                         mutexRelease $ lock q
                                         putStrLn "\tYes"
                                         putStrLn "Success."
                   putStrLn "---  End testMakeQueue  ---\n"


testEnqueue : IO ()
testEnqueue = do putStrLn "--- Begin testEnqueue ---"
                 qRef <- makeQueue
                 q <- readIORef qRef
                 r <- readIORef $ rear q
                 if length r /= 0
                    then putStrLn "ERROR: Rear not initially empty"
                    else do enqueue qRef (prepare "testEnqueue")
                            q' <- readIORef qRef
                            r' <- readIORef $ rear q'
                            if length r' /= 1
                               then putStrLn "ERROR: Did not enqueue 1 item"
                               else putStrLn "Success."
                 putStrLn "---  End testEnqueue  ---\n"


testDequeue : IO ()
testDequeue = do putStrLn "--- Begin testDequeue ---"
                 qRef <- makeQueue
                 q <- readIORef qRef
                 f <- readIORef $ front q
                 r <- readIORef $ rear q
                 if length f /= 0 || length r /= 0
                    then putStrLn "ERROR: Queue not initially empty"
                    else do enqueue qRef (prepare "testDequeue")
                            dq <- dequeue qRef
                            case dq of
                                 Nothing => putStrLn "ERROR: Nothing dequeued"
                                 Just m => case unsafeOpen m String of
                                                "testDequeue" =>
                                                  do q' <- readIORef qRef
                                                     f' <- readIORef $ front q'
                                                     r' <- readIORef $ rear q'
                                                     if length f' /= 0 || length r' /= 0
                                                        then putStrLn "ERROR: Queue not empty afterwards"
                                                        else putStrLn "Success."

                                                _ =>
                                                  putStrLn "ERROR: Did not get same thing back"
                 putStrLn "---  End testDequeue  ---\n"

testPeek : IO ()
testPeek = do putStrLn "--- Begin testPeek ---"
              qRef <- makeQueue
              q <- readIORef qRef
              f <- readIORef $ front q
              r <- readIORef $ rear q
              if length f /= 0 || length r /= 0
                 then putStrLn "ERROR: Queue not initially empty"
                 else do enqueue qRef (prepare "testPeek")
                         pk <- peek qRef
                         case pk of
                              Nothing => putStrLn "ERROR: Peek got Nothing"
                              Just m => case unsafeOpen m String of
                                             "testPeek" =>
                                               do q' <- readIORef qRef
                                                  f' <- readIORef $ front q'
                                                  r' <- readIORef $ rear q'
                                                  if length f' /= 1 && length r' /= 0
                                                     then putStrLn "ERROR: Queue in unexpected state after peek"
                                                     else putStrLn "Success."

                                             _ =>
                                               putStrLn "ERROR: Did not get same thing back"
              putStrLn "---  End testPeek  ---\n"


runAll : IO ()
runAll = do testMakeQueue
            testEnqueue
            testDequeue
            testPeek

main : IO ()
main = runAll
--}

