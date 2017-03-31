# TDA383 Concurrent Programming
Labs made in the course Concurrent Programming in the spring of 2017 at Chalmers University of Technology. Written in Erlang and Java.
## Labs
### CCHAT
The goal of this lab was to create a text-based messaging system called CCHAT. To run CCHAT you need Erlang installed.
- Compile with ```make all```. Start Erlang
- Run ```cchat:start2()```  to start the server and open two clients
 - To set your nick type ```/nick yourname```
 - Connect to server shire with ```/connect shire```
 - Join a chat room with ```/join #chatroom```
 - Type your message and send by clicking enter
 - To leave the chatroom ```/leave```
 - Disconnect from the server with ```/disconnect```
### Trainspotting
The task of this lab was to make two trains run independently of each other, without colliding, by using semaphores and monitors. Written in Java.

Code written by [Johannes Hildén](https://github.com/hildenjohannes) and [Tobias Hallberg](https://github.com/toobew).
