# client_server_chat
## Build
You need to install `lwt` and `logs` modules:  
`opam install lwt logs`  
To compile:  
`dune build`  

## Run
default ip = localhost  
default port = 9000  
To run the server:  
`dune exec client_server_chat server -- --ip [ip] --port [port]`  
To run the client:  
`dune exec client_server_chat client -- --ip [ip] --port [port]`  
or  
`_build/default/bin/main.exe server/client --ip [ip] --port [port]`

