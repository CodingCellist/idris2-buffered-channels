import System

import System.Concurrency.Pipe

{-- TODO: REWRITE EVERYTHING
data MsgType = Syn
             | Ack
             | Syn_Ack

senderError : (senderID : Int) -> (errMsg : String) -> IO ()
senderError senderID errMsg
  = putStrLn $ "S" ++ (show senderID) ++ ": ERROR: " ++ errMsg

receiverError : (receiverID : Int) -> (errMsg : String) -> IO ()
receiverError receiverID errMsg
  = putStrLn $ "R" ++ (show receiverID) ++ ": ERROR: " ++ errMsg

s1 : (chan : Channel) -> IO ()
s1 chan =
  do putStrLn "S1: Hello"
     sendAndSignal chan (prepare Syn)
     putStrLn "S1: Sent 'Syn'"
     rawResp <- await chan
     putStrLn "S1: Got a response"
     let s1Err = senderError 1
     case rawResp of
          Nothing => s1Err "No reply."

          (Just rawResp) => case unsafeOpen rawResp MsgType of
                                 Syn => s1Err "Received 'Syn' as reply."

                                 Ack => do putStrLn "S1: Received 'Ack'"
                                           sendAndSignal chan (prepare Syn_Ack)
                                           putStrLn "S1: Sent 'Syn_Ack'"
                                           putStrLn "S1: -- Done --"

                                 Syn_Ack => s1Err "Received 'Syn_Ack' as reply."

r1 : (chan : Channel) -> IO ()
r1 chan =
  do putStrLn "R1: Hello"
     let r1Err = receiverError 1
     rawMsg <- await chan
     putStrLn "R1: Got an initial message"
     case rawMsg of
          Nothing => r1Err "No initial message."

          (Just msg) =>
            case unsafeOpen msg MsgType of
                 Syn => do putStrLn "R1: Got 'Syn'"
                           sendAndSignal chan (prepare Ack)
                           putStrLn "R1: Sent 'Ack'"
                           rawMsg' <- await chan
                           putStrLn "R1: Got a second message"
                           case rawMsg' of
                                Nothing => r1Err "No second message."

                                (Just msg') =>
                                  case unsafeOpen msg' MsgType of
                                       Syn => r1Err "Got 'Syn' second"

                                       Ack => r1Err "Got 'Ack' second"

                                       Syn_Ack =>
                                         do putStrLn "R1: Got 'Syn_Ack'"
                                            putStrLn "R1: -- Done --"

                 Ack => r1Err "Got 'Ack' first"

                 Syn_Ack => r1Err "Got 'Syn_Ack' first"

testHandshake1 : IO ()
testHandshake1 = do putStrLn "----- Begin Test 1 -----"
                    cRef <- makeChannel
                    sendChan <- makeSender cRef
                    receiveChan <- makeReceiver cRef
                    senderPID <- fork (s1 sendChan)
                    receiverPID <- fork (r1 receiveChan)
                    sleep 3
                    putStrLn "-----  End Test 1  -----"
                    putStrLn ""

testHandshake2 : IO ()
testHandshake2 = do putStrLn "----- Begin Test 2 -----"
                    cRef <- makeChannel
                    sendChan <- makeSender cRef
                    receiveChan <- makeReceiver cRef
                    senderPID <- fork (s1 sendChan)
                    receiverPID <- fork (r1 receiveChan)
                    sleep 3
                    putStrLn "-----  End Test 2  -----"
                    putStrLn ""

main : IO ()
main = do testHandshake1
          testHandshake2
--}
