# idris2-channels

This is an attempt at creating channels in Idris2, as part of enabling
somebody to complete Chapter 15 of the book "Type-Driven Development with
Idris". The API has to be changed, since having type-agnostic channels just
seems like a bad time.

The channels should be MT-Safe and ideally it should be possible to pass them
around between threads (not possible in Idris1 due to inboxes being tied to
processes/threads).

# Overview

Queues are implemented as 2 stacks. In order to keep channels a bit simpler,
Queues are MTSafe (let Queues worry about that rather than channels).

BufferedChannels have a Queue that they can send stuff on, and the send/receive
functions have to be obtained as a DPair.

Pipes have an inbox and an outbox, which are swapped depending on whether a
process is a sender or a receiver

Both BufferedChannels and Pipes support waiting for things to arrive by using
Condition Variables.

