||| A channel built on top of System.Concurrency.Queue. It is MTSafe thanks to
||| Queues being MTSafe, and buffered as it operates using a Queue of things.
module System.Concurrency.BufferedChannel

import System.Concurrency

import public System.Concurrency.Queue

%default total

||| A channel for inter-process communication where one side is the sender, and
||| the other is the receiver.
||| See also:
||| - @makeBufferedChannel@
||| - @becomeSender@
||| - @becomeReceiver@
export
data BufferedChannel : Type -> Type where
  MkBufferedChannel : (condLock : Mutex)
                    -> (condVar : Condition)
                    -> (qRef    : IORef (Queue a))
                    -> BufferedChannel a

||| Create a new BufferedChannel. Since BufferedChannels are shared resources,
||| they can only be obtained in an IORef.
||| See also
||| - @becomeSender@
||| - @becomeReceiver@
export
makeBufferedChannel : IO (IORef (BufferedChannel a))
makeBufferedChannel =
  do cl <- makeMutex
     cv <- makeCondition
     q  <- makeQueue
     newIORef (MkBufferedChannel cl cv q)

||| The type of a sender function is something which takes a channel and a thing
||| to send, and produces an empty IO action, i.e. the sending of the message.
public export
SenderFunc : Type -> Type
SenderFunc a = BufferedChannel a -> a -> IO ()

||| Send a thing on the BufferedChannel and signal its internal condition
||| variable.
|||
||| MTSafe: YES
sendAndSignal : SenderFunc a
sendAndSignal (MkBufferedChannel _ condVar qRef) thing =
  do enqueue qRef thing
     conditionSignal condVar

||| Send a thing on the BufferedChannel and broadcast its internal condition
||| variable.
|||
||| MTSafe: YES
sendAndBroadcast : SenderFunc a
sendAndBroadcast (MkBufferedChannel _ condVar qRef) thing =
  do enqueue qRef thing
     conditionBroadcast condVar

||| The type of a receiver function is something which takes a channel and
||| produces an IO action containing a thing on the channel, i.e. the receiving
||| of the message.
public export
ReceiverFunc : Type -> Type
ReceiverFunc a = BufferedChannel a -> IO a

||| Crash Idris with a message signalling that something went wrong in terms of
||| the fundamental guarantees of condition variables.
|||
||| Essentially, the point of waiting on a CV is to wait until something is
||| available for consumption. So as soon as the CV lets us past, there will be
||| something to retrieve. However, Idris doesn't know this.
await_crash : IO a
await_crash =
  assert_total $ idris_crash "Await somehow got Nothing despite waiting on a CV"

||| Block on the BufferdChannel's internal condition variable until a thing
||| arrives, and at that point, retrieve the thing.
|||
||| MTSafe: YES
await : ReceiverFunc a
await (MkBufferedChannel condLock condVar qRef) =
  do (Just thing) <- dequeue qRef
          -- if there wasn't anything in the queue, wait until something appears
        | Nothing => do mutexAcquire condLock
                        conditionWait condVar condLock
                        (Just thing') <- dequeue qRef
                           | Nothing => await_crash
                        mutexRelease condLock
                        pure thing'
     pure thing

||| Given a reference to a BufferdChannel, obtain the ability to send on the
||| channel.
export
becomeSender : (bcRef : IORef (BufferedChannel a))
             -> IO (dc : BufferedChannel a ** (SenderFunc a))
becomeSender bcRef =
  do theBC <- readIORef bcRef
     pure (MkDPair theBC sendAndSignal)

||| Given a reference to a BufferedChannel, obtain the ability to receive on the
||| channel.
export
becomeReceiver : (bcRef : IORef (BufferedChannel a))
               -> IO (dc : BufferedChannel a ** (ReceiverFunc a))
becomeReceiver bcRef =
  do theBC <- readIORef bcRef
     pure (MkDPair theBC await)

