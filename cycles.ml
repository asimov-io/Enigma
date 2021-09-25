open Symbol
open Graph
open Path

let graph_of_known_cipher (m: string) (c: string) : Graph.t =
  let g = create () in
  let add (index: int) (chr: char) : unit =
    add_edge g (of_char chr) (of_char c.[index]) index
  in String.iteri add m;
  g

let cycles (g: Graph.t) : unit =
  let paths = Array.make nb_syms [] in
  paths.(0) <- List.init nb_syms (fun i -> singleton (of_int i));
  ()
    
(** Behaviour of the "enigma" executable. *)
let () =
  if Filename.basename Sys.argv.(0) = "cycles" then
    let _ = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in ()