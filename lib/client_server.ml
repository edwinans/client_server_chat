open Lwt

let backlog = 10
let write_console = Lwt_io.write_line Lwt_io.stdout
let read_console () = Lwt_io.read_line Lwt_io.stdin
let last_time = ref 0.

let rec send_message oc =
  read_console () >>= fun s ->
  last_time := Unix.gettimeofday ();
  Lwt_io.write_line oc s >>= fun _ -> send_message oc

let rec handle_message ic oc side =
  let open Lwt_io in
  read_line_opt ic >>= function
  | Some "ack" ->
      let roundtrip_delay = Unix.gettimeofday () -. !last_time in
      write_console (Printf.sprintf "[roundtrip delay] : %fms" roundtrip_delay)
      >>= fun _ -> handle_message ic oc side
  | Some msg ->
      write_console ("[" ^ side ^ "]: " ^ msg) >>= fun _ ->
      write_line oc "ack" >>= fun _ -> handle_message ic oc side
  | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return

let handle_connection ic oc =
  Lwt.on_failure (handle_message ic oc "client") (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  Logs_lwt.info (fun m -> m "New connection") >>= return

let create_socket addr port =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let _ = bind sock @@ ADDR_INET (addr, port) in
  listen sock backlog;
  sock

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock
    >>= (fun (fd, _) ->
          let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
          let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
          handle_connection ic oc <&> send_message oc)
    >>= serve
  in
  serve ()

let create_client sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  send_message oc <&> handle_message ic oc "server"

let connect_server addr port =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  Lwt.on_success
    (Lwt_unix.connect sock @@ ADDR_INET (addr, port))
    (fun _ ->
      let _ = write_console "Connected to the server" in
      ());
  sock
