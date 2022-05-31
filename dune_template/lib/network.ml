open Base

open LTerm_style
open LTerm_text


let r1 = Str.regexp "```*"
let r2 = Str.regexp "=>*"

let h1 = Str.regexp "#[ \t]*\\([^#].+\\)"
and h2 = Str.regexp "##[ \t]*\\([^#].+\\)"
and h3 = Str.regexp "###[ \t]*\\([^#].+\\)"


type status =
  | SUCCESS (*20*)
  | WEIRD 

let code_to_status str =
  match str with
  | "20" -> SUCCESS
  | s -> failwith ("unknown status"^s)



(* get the first IP associated to the hostname *)
let get_ipaddress hostname =
  let open Lwt.Syntax in
  let* he = Lwt_unix.gethostbyname hostname in
  Lwt.return (Unix.string_of_inet_addr (he.h_addr_list).(0))


(* create the client socket *)
let create_socket () =
  let sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.set_blocking sock false;
  sock

(* create an SSL/TLS context *)
let sslctx = let () = Ssl.init () in
  Ssl.create_context Ssl.SSLv23 Ssl.Client_context 

(* connect to the capsule, dual way *)
let get_io_channels uri port =
  let open Lwt in
  let hoststring = Uri.host_with_default uri in
  Lwt_io.printf "URI is %s\n" hoststring >>= fun () -> 
  get_ipaddress hoststring  >>= fun ip ->
  Lwt_io.printf "IP is %s\n" ip >>= fun () -> 
  Lwt_unix.getaddrinfo ip port [] >>= fun addresses ->
  let server = Lwt_unix.((List.nth_exn addresses 0).ai_addr) in
  let socket = create_socket () in
  Lwt_unix.connect socket server >>= fun () ->
  Lwt_ssl.ssl_connect socket sslctx >>= fun socket ->
  let ic = Lwt_ssl.in_channel_of_descr socket in
  let oc = Lwt_ssl.out_channel_of_descr socket in
  return (ic, oc)



(* request an URI and print the raw text of the response *)
let get_and_print_raw uri (incoming, outgoing) =
  let module S = String in
  let open Lwt in
  let request = Printf.sprintf "%s\r\n" (Uri.to_string uri) in
  Lwt_io.write outgoing request >>= fun () ->
  Lwt_io.flush_all () >>= fun () ->
  Lwt_io.read_line incoming >>= fun head -> (* first line *)
  Lwt_io.fprintf Lwt_io.stdout "HEADER: %s\n" head >>= fun () ->
  let rec loop incoming = 
    Lwt_io.read_line_opt incoming >>= function
    | Some text->
      if Str.string_match h1 text 0 then  LTerm.printls(eval [
        B_fg(index 9); S (Str.matched_group 1 text); E_fg])>>= fun () -> 
        loop incoming
      else if Str.string_match h2 text 0 then LTerm.printls(eval [
        B_fg(index 10); S (Str.matched_group 1 text); E_fg])>>= fun () -> 
        loop incoming
      else if Str.string_match r2 text 0 then 
        let post = String.sub text ~pos:2 ~len:(String.length text - 2) in
        match Str.(bounded_split (regexp "[ \t]+") post 2) with
          | [ url ] -> LTerm.printls(eval [
            B_fg(index 6); S (url); E_fg])>>= fun () -> 
            loop incoming
          | [ url; name ] -> LTerm.printls(eval [
            B_fg(index 4); S (url); E_fg; S"\t";
            B_fg(index 6); S (name); E_fg])>>= fun () -> 
            loop incoming
          | _ -> loop incoming
      else LTerm.printls(eval [B_fg(index 15); S (text); E_fg])>>= fun () -> 
      loop incoming
    | _ -> Lwt.return ()

  in loop incoming
  
