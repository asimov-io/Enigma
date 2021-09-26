open Symbol
open Graph
open Path

let graph_of_known_cipher (m: string) (c: string) : Graph.t =
  let g = create () in
  let add (index: int) (chr: char) : unit =
    add_edge g (of_char chr) (of_char c.[index]) index
  in String.iteri add m;
  g

let cycles (g: Graph.t) : sym list list =
  let res = ref [] in
  let cycles = Array.make (nb_syms + 1) [] in
  let paths = Array.make nb_syms [] in
  paths.(0) <- List.init nb_syms (fun i -> singleton (of_int i));
  
  let add_symbol_to_path (p: Path.t) ((): unit) (v: sym) : unit =
    let s = source p in
    let len = List.length (rev_path p) in
    if to_int v >= to_int s then begin
      if v == s then
        cycles.(len) <- (snoc p v) :: cycles.(len)
      else begin
        if not (mem p v) then begin
          paths.(len) <- (snoc p v) :: paths.(len)
        end
      end
    end

  in let extend_path (p: Path.t) : unit =
    let l = rev_path p in
    let h = List.hd l in
    fold_over_connected g (add_symbol_to_path p) () h

  in for i = 1 to nb_syms do
    List.iter extend_path paths.(i - 1)
  done;

  let rec remove_doubles (l: Path.t list) : unit = match l with
    | [] -> ()
    | h :: t ->
      let h_path = rev_path h in
      let aux = fun p -> List.rev (rev_path p) <> h_path in
      if (List.for_all aux t) then res := h_path :: !res;
      remove_doubles t
  in Array.iter remove_doubles cycles;
  !res

    
    
(* Behaviour of the "enigma" executable. *)
let () =
  if Filename.basename Sys.argv.(0) = "cycles" then
    let _ = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in ()