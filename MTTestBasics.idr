-- Idris 2

import Queues
import MTSafeChannels

import System
import Data.IORef

senderError : (senderID : Int) -> (errMsg : String) -> IO ()
senderError senderID errMsg =
  putStrLn $ "S" ++ (show senderID) ++ ": ERROR: " ++ errMsg

receiverError : (receiverID : Int) -> (errMsg : String) -> IO ()
receiverError receiverID errMsg =
  putStrLn $ "R" ++ (show receiverID) ++ ": ERROR: " ++ errMsg


testReceiveEmpty : IO ()
testReceiveEmpty =
  do putStrLn "--- Begin testReceiveEmpty ---"
     cRef <- makeChannel
     chan <- readIORef cRef
     rawMsg <- receive chan
     case rawMsg of
          Nothing => putStrLn "Success."
          Just m =>
             putStrLn "ERROR: Got a message from the void"
     putStrLn "---  End testReceiveEmpty  ---\n"


testSendReceive_SENDER : (chan : Channel) -> IO ()
testSendReceive_SENDER chan =
  do sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendReceive_RECEIVER : (chan : Channel) -> IO ()
testSendReceive_RECEIVER chan =
  do rawMsg <- receive chan
     case rawMsg of
          Nothing => receiverError 1 "Received Nothing"
          Just msg => case unsafeOpen msg String of
                           "Hello" => putStrLn "R1: Success."
                           _ => receiverError 1 "Didn't receive 'Hello'"

testSendReceive : IO ()
testSendReceive =
  do putStrLn "--- Begin testSendReceive ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendReceive_SENDER chan_SENDER
     let receiver = testSendReceive_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     sleep 1
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendReceive  ---\n"


testReceiveReply_SENDER : (chan : Channel) -> IO ()
testReceiveReply_SENDER chan =
  do rawReply <- receive chan
     case rawReply of
          Nothing => senderError 1 "Received Nothing as reply"
          Just msg => case unsafeOpen msg String of
                           "World" => putStrLn "S1: Success."
                           _ => senderError 1 "Didn't receive 'World'"

testReceiveReply_RECEIVER : (chan : Channel) -> IO ()
testReceiveReply_RECEIVER chan =
  do sendAndSignal chan (prepare "World")
     putStrLn "R1: Sent 'World' and signalled"

testReceiveReply : IO ()
testReceiveReply =
  do putStrLn "--- Begin testReceiveReply ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testReceiveReply_SENDER chan_SENDER
     let receiver = testReceiveReply_RECEIVER chan_RECEIVER
     -- fork in reverse order for minimal risk of runtime disruption
     pid_RECEIVER <- fork receiver
     sleep 1
     pid_SENDER <- fork sender
     sleep 1
     putStrLn "---  End testReceiveReply  ---\n"


testSendSignalAwait_SENDER : (chan : Channel) -> IO ()
testSendSignalAwait_SENDER chan =
  do sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalAwait_RECEIVER : (chan : Channel) -> IO ()
testSendSignalAwait_RECEIVER chan =
  do rawMsg <- await chan
     case rawMsg of
          Nothing => receiverError 1 "Await got Nothing"
          Just msg => case unsafeOpen msg String of
                           "Hello" => putStrLn "R1: Success."
                           _ => receiverError 1 "Await didn't get 'Hello'"

testSendSignalAwait : IO ()
testSendSignalAwait =
  do putStrLn "--- Begin testSendSignalAwait ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalAwait_SENDER chan_SENDER
     let receiver = testSendSignalAwait_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendSignalAwait  ---\n"


testSendSignalAwait2_SENDER : (chan : Channel) -> IO ()
testSendSignalAwait2_SENDER chan =
  do sleep 1
     sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalAwait2_RECEIVER : (chan : Channel) -> IO ()
testSendSignalAwait2_RECEIVER chan =
  do rawMsg <- await chan
     case rawMsg of
          Nothing => receiverError 1 "Await got Nothing"
          Just msg => case unsafeOpen msg String of
                           "Hello" => putStrLn "R1: Success."
                           _ => receiverError 1 "Got something weird"

-- test that `await` works, as long as there is some update at some point
testSendSignalAwait2 : IO ()
testSendSignalAwait2 =
  do putStrLn "--- Begin testSendSignalAwait2 ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalAwait2_SENDER chan_SENDER
     let receiver = testSendSignalAwait2_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 2
     putStrLn "---  End testSendSignalAwait2  ---\n"


testAwaitTimedOut_RECEIVER : (chan : Channel) -> IO ()
testAwaitTimedOut_RECEIVER chan =
  do timeoutRes <- awaitTimeout chan 500000
     case timeoutRes of
          TimedOut => putStrLn "R1: Success."
          Succeeded msg => receiverError 1 "Got a message from the void"

testAwaitTimedOut : IO ()
testAwaitTimedOut =
  do putStrLn "--- Begin testAwaitTimedOut ---"
     cRef <- makeChannel
     chan_RECEIVER <- makeReceiver cRef
     let receiver = testAwaitTimedOut_RECEIVER chan_RECEIVER
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testAwaitTimedOut  ---\n"


testSendSignalAwaitTimedOut_SENDER : (chan : Channel) -> IO ()
testSendSignalAwaitTimedOut_SENDER chan =
  do sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalAwaitTimedOut_RECEIVER : (chan : Channel) -> IO ()
testSendSignalAwaitTimedOut_RECEIVER chan =
  do timeoutRes <- awaitTimeout chan 500000
     case timeoutRes of
          TimedOut => receiverError 1 "Timed out"
          Succeeded rawMsg => case unsafeOpen rawMsg String of
                                   "Hello" => putStrLn "R1: Success."
                                   _ => receiverError 1 "Didn't get 'Hello'"

testSendSignalAwaitTimedOut : IO ()
testSendSignalAwaitTimedOut =
  do putStrLn "--- Begin testSendSignalAwaitTimedOut ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalAwaitTimedOut_SENDER chan_SENDER
     let receiver = testSendSignalAwaitTimedOut_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendSignalAwaitTimedOut  ---\n"


testSendSignalAwaitTooSlow_SENDER : (chan : Channel) -> IO ()
testSendSignalAwaitTooSlow_SENDER chan =
  do sleep 1
     sendAndSignal chan (prepare "Sorry I'm late")
     putStrLn "S1: After 1 second, sent \"Sorry I'm late\" and signalled"

testSendSignalAwaitTooSlow_RECEIVER : (chan : Channel) -> IO ()
testSendSignalAwaitTooSlow_RECEIVER chan =
  do timeoutRes <- awaitTimeout chan 500000
     case timeoutRes of
          TimedOut => putStrLn "R1: Success."
          Succeeded rawMsg => receiverError 1 "Somehow got a message."

testSendSignalAwaitTooSlow : IO ()
testSendSignalAwaitTooSlow =
  do putStrLn "--- Begin testSendSignalAwaitTooSlow ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalAwaitTooSlow_SENDER chan_SENDER
     let receiver = testSendSignalAwaitTooSlow_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 2
     putStrLn "---  End testSendSignalAwaitTooSlow  ---\n"


testSendSignalPeek_SENDER : (chan : Channel) -> IO ()
testSendSignalPeek_SENDER chan =
  do sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalPeek_RECEIVER : (chan : Channel) -> IO ()
testSendSignalPeek_RECEIVER chan =
  do maybePeek <- peek chan
     case maybePeek of
          Nothing => receiverError 1 "Peek got Nothing"
          Just rawMsg => case unsafeOpen rawMsg String of
                              -- `peek` internals, i.e. msg isn't accidentally
                              -- dequeued, tested in MTTestQueues.idr
                              "Hello" => putStrLn "R1: Success."
                              _ => receiverError 1 "Did not get 'Hello'"

testSendSignalPeek : IO ()
testSendSignalPeek =
  do putStrLn "--- Begin testSendSignalPeek ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalPeek_SENDER chan_SENDER
     let receiver = testSendSignalPeek_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     sleep 1
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendSignalPeek  ---\n"


testSendSignalSpy_SENDER : (chan : Channel) -> IO ()
testSendSignalSpy_SENDER chan =
  do sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalSpy_RECEIVER : (chan : Channel) -> IO ()
testSendSignalSpy_RECEIVER chan =
  do rawMsg <- spy chan
     case rawMsg of
          Nothing => receiverError 1 "Spy got Nothing"
          Just msg => case unsafeOpen msg String of
                           "Hello" => putStrLn "R1: Success."
                           _ => receiverError 1 "Spy didn't get 'Hello'"

testSendSignalSpy : IO ()
testSendSignalSpy =
  do putStrLn "--- Begin testSendSignalSpy ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalSpy_SENDER chan_SENDER
     let receiver = testSendSignalSpy_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendSignalSpy  ---\n"


testSendSignalSpy2_SENDER : (chan : Channel) -> IO ()
testSendSignalSpy2_SENDER chan =
  do sleep 1
     sendAndSignal chan (prepare "Hello")
     putStrLn "S1: Sent 'Hello' and signalled"

testSendSignalSpy2_RECEIVER : (chan : Channel) -> IO ()
testSendSignalSpy2_RECEIVER chan =
  do maybeMsg <- spy chan
     case maybeMsg of
          Nothing => receiverError 1 "Spy got Nothing"
          Just msg => case unsafeOpen msg String of
                           "Hello" => putStrLn "R1: Success."
                           _ => receiverError 1 "Spy didn't get 'Hello'"

testSendSignalSpy2 : IO ()
testSendSignalSpy2 =
  do putStrLn "--- Begin testSendSignalSpy2 ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalSpy2_SENDER chan_SENDER
     let receiver = testSendSignalSpy2_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 2
     putStrLn "---  End testSendSignalSpy2  ---\n"


testSendSignalSpyTimedOut_RECEIVER : (chan : Channel) -> IO ()
testSendSignalSpyTimedOut_RECEIVER chan =
  do timeoutRes <- spyTimeout chan 500000
     case timeoutRes of
          TimedOut => putStrLn "R1: Success."
          Succeeded msg => receiverError 1 "Spy got a message from the void"

testSendSignalSpyTimedOut : IO ()
testSendSignalSpyTimedOut =
  do putStrLn "--- Begin testSendSignalSpyTimedOut ---"
     cRef <- makeChannel
     chan_RECEIVER <- makeReceiver cRef
     let receiver = testSendSignalSpyTimedOut_RECEIVER chan_RECEIVER
     pid_RECEIVER <- fork receiver
     sleep 1
     putStrLn "---  End testSendSignalSpyTimedOut  ---\n"


testSendSignalSpyTooSlow_SENDER : (chan : Channel) -> IO ()
testSendSignalSpyTooSlow_SENDER chan =
  do sleep 1
     sendAndSignal chan (prepare "I'm late! I'm late!")
     putStrLn "S1: After 1 second, sent a message and signalled"

testSendSignalSpyTooSlow_RECEIVER : (chan : Channel) -> IO ()
testSendSignalSpyTooSlow_RECEIVER chan =
  do timeoutRes <- spyTimeout chan 500000
     case timeoutRes of
          TimedOut => putStrLn "R1: Success."
          Succeeded rawMsg => receiverError 1 "Got a message despite impatience"

testSendSignalSpyTooSlow : IO ()
testSendSignalSpyTooSlow =
  do putStrLn "--- Begin testSendSignalSpyTooSlow ---"
     cRef <- makeChannel
     chan_SENDER <- makeSender cRef
     chan_RECEIVER <- makeReceiver cRef
     let sender = testSendSignalSpyTooSlow_SENDER chan_SENDER
     let receiver = testSendSignalSpyTooSlow_RECEIVER chan_RECEIVER
     pid_SENDER <- fork sender
     pid_RECEIVER <- fork receiver
     sleep 2
     putStrLn "---  End testSendSignalSpyTooSlow  ---\n"


runAll : IO ()
runAll = do testReceiveEmpty
            testSendReceive
            testReceiveReply
            testSendSignalAwait
            testSendSignalAwait2
            testAwaitTimedOut
            testSendSignalAwaitTimedOut
            testSendSignalAwaitTooSlow
            testSendSignalPeek
            testSendSignalSpy
            testSendSignalSpy2
            testSendSignalSpyTimedOut
            testSendSignalSpyTooSlow


main : IO ()
main = runAll

