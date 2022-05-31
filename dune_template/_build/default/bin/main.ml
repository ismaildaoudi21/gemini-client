(* open Base *)
open Ocamlgemini
open Unit
open Lwt.Infix 



let () = 
  let rec loop () = 
    let open Lwt_io in
        (print "Entrez un lien Ou quitter avec 'exit': " >>=
        fun () -> read_line stdin >>= fun line ->
        if line = "exit" then
        exit 0
        else 
          let reply = Demo.print_homepage line in
          print reply) >>= fun _ ->
        let uri = Uri.of_string !Demo.homepage in
        Network.get_io_channels uri "1965" >>= (* TCP connexion *)
        Network.get_and_print_raw uri >>= loop 
        in Lwt_main.run(loop())
