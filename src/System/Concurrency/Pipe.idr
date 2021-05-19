||| A channel with a dedicated read and write end, built on top of
||| System.Concurrency.Queue. It is MTSafe thanks to Queues being MTSafe.
module System.Concurrency.Pipe

import System.Concurrency

import public System.Concurrency.Queue

||| A channel whose read end has type `rdT` and whose write end has type `wrT`.
export
data Pipe : (rdT : Type) -> (wrT : Type) -> Type where
  MkPipe : {0 a : Type} -> {0 b : Type}
         -> (inbox  : IORef (Queue a))
         -> (iCondLock : Mutex)
         -> (iCondVar : Condition)
         -> (outbox : IORef (Queue b))
         -> (oCondLock : Mutex)
         -> (oCondVar : Condition)
         -> Pipe a b


---------------------------
-- CONSTRUCTOR FUNCTIONS --
---------------------------

||| Create a new Pipe.
export
makePipe : {rdT : Type} -> {wrT : Type}
         -> IO (IORef (Pipe rdT wrT))
makePipe = do iQueue <- makeQueue
              iCondLock <- makeMutex
              iCondVar <- makeCondition
              -- ^ inbox/read things
              -- v outbox/write things
              oQueue <- makeQueue
              oCondLock <- makeMutex
              oCondVar <- makeCondition
              -- create the pipe in an IORef
              newIORef (MkPipe iQueue iCondLock iCondVar
                               oQueue oCondLock oCondVar)

||| Given an IORef to a Pipe, obtained through the `makePipe` function, create a
||| version which is primarily used for sending messages.
export
makeSender : (pRef : IORef (Pipe rdT wrT)) -> IO (Pipe rdT wrT)
makeSender pRef
  = do (MkPipe inbox iCL iCV outbox oCL oCV) <- readIORef pRef
       pure (MkPipe inbox iCL iCV outbox oCL oCV)

||| Given an IORef to a Pipe, obtained through the `makePipe` function, create a
||| version which is primarily used for receiving messages, possibly responding
||| on what appears as the `rd` end to the other process.
export
makeReceiver : (pRef : IORef (Pipe rdT wrT)) -> IO (Pipe wrT rdT)
makeReceiver pRef
  = do (MkPipe inbox iCL iCV outbox oCL oCV) <- readIORef pRef
       pure (MkPipe outbox oCL oCV inbox iCL iCV)


------------------------
-- INBOX MANIPULATION --
------------------------

getInbox : Pipe a b -> IORef (Queue a)
getInbox (MkPipe inbox _ _ _ _ _) = inbox


-------------------------
-- OUTBOX MANIPULATION --
-------------------------

getOutbox : Pipe a b -> IORef (Queue b)
getOutbox (MkPipe _ _ _ outbox _ _) = outbox

||| Unlock at least one of the threads currently waiting on the outbox's
||| condition variable.
signalOutbox : Pipe a b -> IO ()
signalOutbox (MkPipe _ _ _ _ _  oCondVar)
  = conditionSignal oCondVar

||| Unlock all of the threads currently waiting on the outbox's condition
||| variable.
broadcastOutbox : Pipe a b -> IO ()
broadcastOutbox (MkPipe _ _ _ _ _ oCondVar)
  = conditionBroadcast oCondVar


-------------
-- SENDING --
-------------

||| Send a thing through the Pipe and *signal* on the condition variable for the
||| Pipe's outbox to indicate to at least one blocking thread blocking on it,
||| that a thing can be received.
|||
||| @chan: The Pipe to send the thing through.
||| @thing: The thing to send.
|||
||| MTSafe: YES
export
sendAndSignal : (chan : Pipe rdT wrT) -> (thing : wrT) -> IO ()
sendAndSignal chan thing =
  do enqueue (getOutbox chan) thing
     signalOutbox chan

||| Send a thing through the Pipe and *broadcast* on the condition variable for
||| the Pipe's outbox to indicate to all threads currently blocking on it, that
||| a thing can be received.
|||
||| @chan: The Pipe to send the thing through.
||| @thing: The thing to send.
|||
||| MT-Safe: YES
export
sendAndBroadcast : (chan : Pipe rdT wrT) -> (thing : wrT) -> IO ()
sendAndBroadcast chan thing =
  do enqueue (getOutbox chan) thing
     broadcastOutbox chan


---------------
-- RECEIVING --
---------------

||| Crash Idris with a message signalling that something went wrong in terms of
||| the fundamental guarantees of condition variables.
|||
||| Essentially, the point of waiting on a CV is to wait until something is
||| available for consumption. So as soon as the CV lets us past, there will be
||| something to retrieve. However, Idris doesn't know this.
await_crash : IO a
await_crash =
  assert_total $ idris_crash "Await somehow got Nothing despite waiting on a CV"

||| Receive a thing through the Pipe, if there is one, removing it from the
||| inbox in the process. Does not block but immediately returns `Nothing` if
||| there was nothing available.
|||
||| @chan: The Pipe to receive the thing through.
|||
||| MT-Safe: YES
export
receive : (chan : Pipe rdT wrT) -> IO (Maybe rdT)
receive chan = dequeue (getInbox chan)

||| Similar to `receive`, but blocks on the condition variable of the inbox
||| until a thing is available, at which point it receives the thing, removing
||| it from the inbox in the process.
|||
||| @chan: The Pipe to receive the thing through.
|||
||| MT-Safe: YES
export
await : (chan : Pipe rdT wrT) -> IO rdT
await (MkPipe inbox iCondLock iCondVar _ _ _) =
  do (Just thing) <- dequeue inbox
          -- if at first you don't succeed...
        | Nothing => do mutexAcquire iCondLock
                        conditionWait iCondVar iCondLock
                        (Just thing') <- dequeue inbox
                            | Nothing => await_crash
                        mutexRelease iCondLock
                        -- ^  FIXME: necessary? Yes. Right place for it?
                        pure thing'

     pure thing


--------------
-- PEEK+SPY --
--------------

||| Receive a thing through the Pipe, if there is one, *without* removing
||| from the inbox in the process. Does not block but immediately returns
||| `Nothing` if there was nothing available.
|||
||| @chan: The Pipe to receive the thing through.
|||
||| MT-Safe: YES
export
peek : (chan : Pipe rdT wrT) -> IO (Maybe rdT)
peek (MkPipe inbox _ _ _ _ _) = peek inbox

||| Watches the Pipe until a thing appears, at which point it "reports back"
||| with the thing that arrived. Hence, `spy`.
|||
||| Similar to `peek`, but blocks on the condition variable of the inbox until a
||| thing is available, at which point it receives the thing, *without* removing
||| it from the Pipe's inbox in the process.
|||
||| @chan: The Pipe to receive the thing through.
|||
||| MT-Safe: YES
export
spy : (chan : Pipe rdT wrT) -> IO rdT
spy (MkPipe inbox iCondLock iCondVar _ _ _) =
  do (Just thing) <- peek inbox
        | Nothing => do mutexAcquire iCondLock
                        conditionWait iCondVar iCondLock
                        (Just thing') <- peek inbox
                            | Nothing => await_crash
                        mutexRelease iCondLock
                        -- ^  FIXME: necessary? Yes. Right place for it?
                        pure thing'
     pure thing

