# idris2-channels

This is an attempt at creating message channels in Idris2, as part of enabling
somebody to complete Chapter 15 of the book "Type-Driven Development with
Idris".

The channels should be MT-Safe and ideally it should be possible to pass them
around between threads (not possible in Idris1 due to inboxes being tied to
processes/threads).

# Overview

Queues are implemented as 2 stacks. In order to keep Channels a bit simpler,
Queues are MT-Safe (let Queues worry about that rather than Channels). Channels
have an inbox and an outbox, which are swapped depending on whether the process
is a sender or a receiver, and support waiting for messages to arrive by using
condition variables.

