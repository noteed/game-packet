# game-packet

Simple UDP-based game networking

Game-packet is a Haskell library providing a simple networking stack for
fast-paced action games (e.g. first-person shooters). The principles are
from [gafferongames.com][].

[gafferongames.com]: http://gafferongames.com/

## Principles

At the base of the stack is the Socket. A socket has no notion of connection.
It just lets you send to and receive packet from (either a ByteString or more
generally a Serialize instance) another socket.

Above the Socket is the Connection. A connection is defined by a steady stream
of packets. When packets don't flow for more than some period of time, the
connection is broken.

Above the Connection, Acknowledgement is built. Acknowledgement provides
sequence numbers and acks. It is not yet implemented.

