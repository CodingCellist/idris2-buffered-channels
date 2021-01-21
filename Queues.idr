-- Idris 2

module Queues

import Data.List
import Data.IORef
import System.Concurrency.Raw

-- Box, but sensibly named
export
record Message where
  constructor MkMessage
  contents : ty

||| Prepare something for sending by putting it in a Message. Note that there is
||| no way to safely retrieve items from a Message once they're in there!
|||
||| @item: the thing to put in the message
export
prepare : (item : ty) -> Message
prepare item = MkMessage item

||| Open a message, expecting it to contain something of `expectedType`. Note
||| however, that we cannot possibly guarantee that the message actually
||| contains this, since it has come from the outside (hence 'unsafe').
|||
||| @msg: the message to open
||| @expectedType: the Type that the contents are assumed to have
export
unsafeOpen : Message -> (expectedType : Type) -> expectedType
unsafeOpen (MkMessage contents) expectedType = believe_me contents

-- FIXME: This doesn't work for some reason...
--unsafeOpen : (msg : Message) -> (expectedType : Type) -> (item : expectedType)
--unsafeOpen : Message -> (expectedType : Type) -> (item : expectedType)

-- FIXME: Shouldn't have to export this (or at least, should only export for
--        test reasons)
public export
Stack : Type
Stack = List Message

emptyStack : Stack
emptyStack = []

makeStack : IO (IORef Stack)
makeStack = newIORef emptyStack

namespace Stack
  ||| Put something at the top of the stack.
  |||
  ||| MT-Safe: NO
  export
  push : (sRef : IORef Stack) -> (msg : Message) -> IO ()
  push sRef msg = modifyIORef sRef ((::) msg)

  ||| Get the first item on the stack if there is one, removing it in the
  ||| process.
  |||
  ||| MT-Safe: NO
  export
  pop : (sRef : IORef Stack) -> IO (Maybe Message)
  pop sRef = do stack <- readIORef sRef
                case stack of
                     []        => pure Nothing
                     (m :: ms) => do writeIORef sRef ms
                                     pure (Just m)

  ||| Get the first item on the stack if there is one, without removing it.
  |||
  ||| MT-Safe: NO
  export
  peek : (sRef : IORef Stack) -> IO (Maybe Message)
  peek sRef = do stack <- readIORef sRef
                 case stack of
                      []        => pure Nothing
                      (m :: ms) => pure (Just m)

public export
record Queue where
  constructor MkQueue
  front : IORef Stack
  rear  : IORef Stack
  lock  : Mutex

||| Create a new, empty queue.
export
makeQueue : IO (IORef Queue)
makeQueue = do qLock <- makeMutex
               fRef <- makeStack
               rRef <- makeStack
               newIORef (MkQueue fRef rRef qLock)

namespace Queue
  ||| Lock the queue by acquiring its mutex.
  ||| (ONLY FOR INTERNAL USE)
  |||
  ||| MT-Safe: NO
  lockQueue : Queue -> IO ()
  lockQueue (MkQueue _ _ lock) = mutexAcquire lock

  ||| Unlock the queue by releasing its mutex.
  ||| (ONLY FOR INTERNAL USE)
  |||
  ||| MT-Safe: NO
  unlockQueue : Queue -> IO ()
  unlockQueue (MkQueue _ _ lock) = mutexRelease lock

  ||| Move the rear to the front. Used when we're out of items in the front and
  ||| there may be items in the rear.
  ||| (ONLY FOR INTERNAL USE)
  |||
  ||| MT-Safe: NO
  reorder : (fRef : IORef Stack) -> (rRef : IORef Stack) -> IO ()
  reorder fRef rRef = do r <- readIORef rRef
                         case r of
                              [] => pure ()   -- no point in reordering
                              xs => do writeIORef fRef (reverse xs)
                                       writeIORef rRef emptyStack

  ||| Put a message at the end of the queue.
  |||
  ||| MT-Safe: YES
  export
  enqueue : (qRef : IORef Queue) -> (msg : Message) -> IO ()
  enqueue qRef msg = do queue <- readIORef qRef
                        lockQueue queue
                        push (rear queue) msg
                        unlockQueue queue

  ||| Get a message from the queue if there is one, removing it in the process.
  |||
  ||| MT-Safe: YES
  export
  dequeue : (qRef : IORef Queue) -> IO (Maybe Message)
  dequeue qRef
    = do queue <- readIORef qRef
         lockQueue queue
         let fRef = front queue
         let rRef = rear queue
         maybeFront <- pop fRef
         case maybeFront of
              Nothing => do reorder fRef rRef
                            maybeFront' <- pop fRef
                            case maybeFront' of
                                 Nothing => do unlockQueue queue
                                               pure Nothing

                                 (Just msg') => do unlockQueue queue
                                                   pure (Just msg')

              (Just msg) => do unlockQueue queue
                               pure (Just msg)

  ||| Get a message from the queue if there is one, without removing it.
  |||
  ||| MT-Safe: YES
  export
  peek : (qRef : IORef Queue) -> IO (Maybe Message)
  peek qRef
    = do queue <- readIORef qRef
         lockQueue queue
         let fRef = front queue
         let rRef = rear queue
         maybeFront <- peek fRef
         case maybeFront of
              Nothing => do reorder fRef rRef
                            maybeFront' <- peek fRef
                            case maybeFront' of
                                 Nothing => do unlockQueue queue
                                               pure Nothing

                                 (Just msg') => do unlockQueue queue
                                                   pure (Just msg')

              (Just msg) => do unlockQueue queue
                               pure (Just msg)

