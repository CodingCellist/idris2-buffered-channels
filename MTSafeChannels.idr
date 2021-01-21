-- Idris 2

module MTSafeChannels

import Queues

import Data.IORef
import System.Concurrency.Raw

export
record Channel where
  constructor MkChannel
  inbox : IORef Queue
-- Queues are already MT-Safe
--  iLock : Mutex

  iCondLock : Mutex
  iCondVar : Condition

  outbox : IORef Queue
-- Queues are already MT-Safe
--  oLock : Mutex

  oCondLock : Mutex
  oCondVar : Condition


---------------------------
-- CONSTRUCTOR FUNCTIONS --
---------------------------

||| Create a new Channel.
export
makeChannel : IO (IORef Channel)
makeChannel = do iQueue <- makeQueue
                 iCondLock <- makeMutex
                 iCondVar <- makeCondition
                 -- ^ inbox things
                 -- v outbox things
                 oQueue <- makeQueue
                 oCondLock <- makeMutex
                 oCondVar <- makeCondition
                 -- create the channel in an IORef
                 newIORef (MkChannel iQueue iCondLock iCondVar
                                     oQueue oCondLock oCondVar)

||| Given an IORef to a Channel, obtained through the `makeChannel` function,
||| create a version which is primarily used for sending messages.
export
makeSender : (cRef : IORef Channel) -> IO Channel
makeSender cRef
  = do (MkChannel inbox iCL iCV outbox oCL oCV) <- readIORef cRef
       pure (MkChannel inbox iCL iCV outbox oCL oCV)

||| Given an IORef to a Channel, obtained through the `makeChannel` function,
||| create a version which is primarily used for receiving messages.
export
makeReceiver : (cRef : IORef Channel) -> IO Channel
makeReceiver cRef
  = do (MkChannel inbox iCL iCV outbox oCL oCV) <- readIORef cRef
       pure (MkChannel outbox oCL oCV inbox iCL iCV)


------------------------
-- INBOX MANIPULATION --
------------------------

-- Queues are already MT-Safe
--lockInbox : Channel -> IO ()
--
--unlockInbox : Channel -> IO ()

-- I don't think there is ever a reason to do this...
--signalInbox : Channel -> IO ()

||| Wait on the condition variable for the Channel's inbox.
waitInbox : Channel -> IO ()
waitInbox (MkChannel inbox iCondLock iCondVar _ _ _)
  = do 
       conditionWait iCondVar iCondLock

-- OLD IMPLEMENTATION
--  = conditionWait iCondVar iCondLock

||| Wait on the condition variable for the Channel's inbox, with a timeout in
||| micro-seconds (e-6 s).
waitInboxTimeout : Channel -> (uSecs : Int) -> IO ()
waitInboxTimeout (MkChannel inbox iCondLock iCondVar _ _ _) uSecs
  = conditionWaitTimeout iCondVar iCondLock uSecs


-------------------------
-- OUTBOX MANIPULATION --
-------------------------

-- Queues are already MT-Safe
--lockOutbox : Channel -> IO ()
--
--unlockOutbox : Channel -> IO ()

||| Unlock at least one of the threads currently waiting on the outbox's
||| condition variable.
|||
||| On a *nix system, see `man 3 pthread_cond_broadcast` for more details
||| regarding the difference between this function and the `broadcastOutbox`
||| function.
signalOutbox : Channel -> IO ()
signalOutbox (MkChannel _ _ _ outbox oCondLock oCondVar)
  = conditionSignal oCondVar

||| Unlock all of the threads currently waiting on the outbox's condition
||| variable.
|||
||| On a *nix system, see `man 3 pthread_cond_broadcast` for more details
||| regarding the difference between this function and the `signalOutbox`
||| function.
broadcastOutbox : Channel -> IO ()
broadcastOutbox (MkChannel _ _ _ outbox oCondLock oCondVar)
  = conditionBroadcast oCondVar

-- I don't think there is ever a reason to do this...
--waitOutbox : Channel -> IO ()
--
--waitOutboxTimeout : Channel -> (uSecs : Int) -> IO ()


-------------
-- SENDING --
-------------

||| Send a Message through the Channel and *signal* on the condition variable
||| for the Channel's outbox to indicate to at least one blocking thread
||| blocking on it, that a Message can be received (see `signalOutbox`).
|||
||| @chan: The Channel to send the Message through.
||| @msg: The Message to send.
|||
||| MT-Safe: YES
export
sendAndSignal : (chan : Channel) -> (msg : Message) -> IO ()
sendAndSignal chan msg = do enqueue (outbox chan) msg
                            signalOutbox chan

||| Send a Message through the Channel and *broadcast* on the condition variable
||| for the Channel's outbox to indicate to all threads currently blocking on
||| it, that a Message can be received (see `signalOutbox`).
|||
||| @chan: The Channel to send the Message through.
||| @msg: The Message to send.
|||
||| MT-Safe: YES
export
sendAndBroadcast : (chan : Channel) -> (msg : Message) -> IO ()
sendAndBroadcast chan msg = do enqueue (outbox chan) msg
                               broadcastOutbox chan

---------------
-- RECEIVING --
---------------

||| Receive a Message through the Channel, if there is one, removing it from the
||| inbox in the process. Does not block but immediately returns `Nothing` if
||| there was no message available.
|||
||| @chan: The Channel to receive the Message through.
|||
||| MT-Safe: YES
export
receive : (chan : Channel) -> IO (Maybe Message)
receive chan = dequeue (inbox chan)

||| Similar to `receive`, but blocks on the condition variable of the inbox
||| until a Message is available, at which point it receives the Message,
||| removing it from the inbox in the process.
|||
||| @chan: The Channel to receive the Message through.
|||
||| MT-Safe: YES
export
await : (chan : Channel) -> IO (Maybe Message)
await (MkChannel inbox iCondLock iCondVar _ _ _) =
  do maybeMsg <- dequeue inbox
     case maybeMsg of
          -- if at first you don't succeed...
          Nothing => do mutexAcquire iCondLock   -- see man 3 pthread_cond_await
                        conditionWait iCondVar iCondLock
                        rawMsg <- dequeue inbox
                        mutexRelease iCondLock   -- FIXME: necessary? Yes. Right place for it?
                        pure rawMsg
          justMsg => pure justMsg

-- OLD IMPLEMENTATION
--await (MkChannel inbox iCondLock iCondVar _ _ _) =
--  do mutexAcquire iCondLock   -- see man 3 pthread_cond_await
--     conditionWait iCondVar iCondLock
--     rawMsg <- dequeue inbox
--     mutexRelease iCondLock   -- FIXME: necessary? Yes. Right place for it?
--     pure rawMsg

-- OLD OLD IMPLEMENTATION
--  do waitInbox chan
--     dequeue (inbox chan)

-- Basically `Maybe`. But more intuitive for the use-case below?...
-- TODO: Change use-cases to `Either`?
public export
data TimeoutResult = TimedOut
                   --| Succeeded result
                   | Succeeded Message

||| Similar to `receive`, but blocks on the condition variable of the inbox
||| until either a Message is available or the timeout was reached.
||| If a Message was received within the time limit, `Succeeded` is returned,
||| containing the Message received and removing it from the Channel's inbox in
||| the process.
||| Otherwise, if no Message was received within the time limit, returns
||| `TimedOut`.
|||
||| @chan: The Channel to receive the Message through.
||| @uSecs: The time limit in micro-seconds (e-6 s).
|||
||| MT-Safe: YES
export
awaitTimeout : (chan : Channel) -> (uSecs : Int) -> IO TimeoutResult
awaitTimeout (MkChannel inbox iCondLock iCondVar _ _ _) uSecs =
  do maybeMsg <- dequeue inbox
     case maybeMsg of
          Nothing =>  do mutexAcquire iCondLock
                         conditionWaitTimeout iCondVar iCondLock uSecs
                         anyway <- dequeue inbox
                         mutexRelease iCondLock   -- FIXME: Right place for it?
                         case anyway of
                              Nothing => pure TimedOut
                              Just rawMsg => pure (Succeeded rawMsg)
          Just rawMsg => pure (Succeeded rawMsg)

-- OLD IMPLEMENTATION
--awaitTimeout (MkChannel inbox iCondLock iCondVar _ _ _) uSecs =
--  do mutexAcquire iCondLock
--     conditionWaitTimeout iCondVar iCondLock uSecs
--     anyway <- dequeue inbox
--     mutexRelease iCondLock   -- FIXME: Right place for it?
--     case anyway of
--          Nothing => pure TimedOut
--          Just msg => pure (Succeeded msg)

-- OLD OLD IMPLEMENTATION
--awaitTimeout chan uSecs = do waitInboxTimeout chan uSecs
--                             anyway <- dequeue (inbox chan)
--                             case anyway of
--                                  Nothing    => pure TimedOut
--                                  (Just msg) => pure (Succeeded msg)
--

--------------
-- PEEK/SPY --
--------------

||| Receive a Message through the Channel, if there is one, *without* removing
||| from the inbox in the process. Does not block but immediately returns
||| `Nothing` if there was no message available.
|||
||| @chan: The Channel to receive the Message through.
|||
||| MT-Safe: YES
export
peek : (chan : Channel) -> IO (Maybe Message)
peek chan = peek (inbox chan)

||| Watches the Channel until a Message appears, at which point it "reports
||| back" with the Message. Hence, `spy`.
|||
||| Similar to `peek`, but blocks on the condition variable of the inbox until
||| a Message is available, at which point it receives the Message, *without*
||| removing it from the Channel's inbox in the process.
|||
||| @chan: The Channel to receive the Message through.
|||
||| MT-Safe: YES
export
spy : (chan : Channel) -> IO (Maybe Message)
spy (MkChannel inbox iCondLock iCondVar _ _ _) =
  do maybeMsg <- peek inbox
     case maybeMsg of
          Nothing => do mutexAcquire iCondLock
                        conditionWait iCondVar iCondLock
                        rawMsg <- peek inbox
                        mutexRelease iCondLock
                        pure rawMsg
          justMsg => pure justMsg

-- OLD IMPLEMENTATION
--spy chan =
--  do waitInbox chan
--     peek (inbox chan)

||| Similar to `peek`, but watches the Channel (blocking on its condition
||| variable) until either a Message appears or the timeout was reached.
||| If a Message arrived in the Channel's inbox, it returns a `Succeeded`
||| containing the Message, *without* removing it from the Channel's inbox in
||| the process.
||| Otherwise, if no Message was received within the time limit, returns
||| `TimedOut`.
|||
||| @chan: The Channel to receive the Message through.
||| @uSecs: The time limit in micro-seconds (e-6 s).
|||
||| MT-Safe: YES
export
spyTimeout : (chan : Channel) -> (uSecs : Int) -> IO (TimeoutResult)
spyTimeout (MkChannel inbox iCondLock iCondVar _ _ _) uSecs =
  do maybeMsg <- peek inbox
     case maybeMsg of
          Nothing => do mutexAcquire iCondLock
                        conditionWaitTimeout iCondVar iCondLock uSecs
                        anyway <- peek inbox
                        mutexRelease iCondLock
                        case anyway of
                             Nothing => pure TimedOut
                             Just rawMsg => pure (Succeeded rawMsg)
          Just rawMsg => pure (Succeeded rawMsg)

-- OLD IMPLEMENTATION
--spyTimeout chan uSecs = do waitInboxTimeout chan uSecs
--                           anyway <- peek (inbox chan)
--                           case anyway of
--                                Nothing    => pure TimedOut
--                                (Just msg) => pure (Succeeded msg)

