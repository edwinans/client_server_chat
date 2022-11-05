open Client_server_chat.Client_server

let ip = ref Unix.inet_addr_loopback
let port = ref 9000

(* mode = false for client, true for server *)
let mode = ref false

let run_server () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = create_socket !ip !port in
  let serve = create_server sock in
  Lwt_main.run @@ serve

let run_client () =
  print_int !port;
  let sock = connect_server !ip !port in
  let cli = create_client sock in
  Lwt_main.run cli

let speclist =
  [
    ("--ip", Arg.String (fun i -> ip := Unix.inet_addr_of_string i), "set ip");
    ("--port", Arg.Set_int port, "set port");
  ]

let usage_msg = "main [server | client] --ip [ip] --port [port]"

let () =
  Arg.parse speclist (function "server" -> mode := true | _ -> ()) usage_msg;
  if !mode then run_server () else run_client ()