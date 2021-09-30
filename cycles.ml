open Symbol
open Graph
open Path

type cycle = sym array * Positions.t array

let graph_of_known_cipher (m: string) (c: string) : Graph.t =
  let g = create () in
  let add (index: int) (chr: char) : unit =
    add_edge g (of_char chr) (of_char c.[index]) index
  in String.iteri add m;
  g

let cycles (g: Graph.t) : cycle list =
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

  let cycle_from_path (p: Path.t) : cycle =
    let l = rev_path p in
    let n = List.length l in
    let v = Array.make n (of_int 0)in
    let e = Array.make (n - 1) Positions.empty in
    let rec aux i l = match l with
      | [] -> ()
      | [s] -> v.(i) <- s
      | h1 :: h2 :: t -> v.(i) <- h1; e.(i) <- get_edge g h1 h2; aux (i + 1) (h2 :: t)
    in aux 0 l;
    (v, e)
  
  in let rec remove_doubles (l: Path.t list) : unit = match l with
    | [] -> ()
    | h :: t ->
      let h_path = rev_path h in
      let aux = fun p -> List.rev (rev_path p) <> h_path in
      if (List.for_all aux t) then res := cycle_from_path h :: !res;
      remove_doubles t
  in Array.iter remove_doubles cycles;
  !res

let print_cycle (c: cycle) : unit =
  let v, e = c in
  let n = Array.length v in
  for i = 0 to n - 2 do
    Printf.fprintf stdout "%c -{ " (to_char v.(i));
    Positions.iter (fun j -> Printf.fprintf stdout "%i " j) e.(i);
    Printf.fprintf stdout "}-> ";
  done;
  Printf.fprintf stdout "%c\n" (to_char v.(n - 1))


let iter_fusion (f: cycle -> 'a) (l: cycle list) : unit =
  List.iter (fun c -> let _ = f c in ()) l

let develop (c: cycle) : cycle list =
  let v, e = c in
  let n = Array.length v in
  let res = ref [] in

  let rec aux i (acc_e: int list) : unit = match i with
    | _ when i = n - 1 -> res := (v, Array.of_list (List.rev_map (fun j -> Positions.singleton j) acc_e)) :: !res
    | _ -> let set = e.(i) in Positions.iter (fun j -> aux (i + 1) (j :: acc_e)) set
  in aux 0 [];
  !res

let iter_multi (f: cycle -> 'a) (l: cycle list) : unit =
  List.iter (fun c -> iter_fusion f (develop c)) l

(* Behaviour of the "enigma" executable. *)
let () =
  if Filename.basename Sys.argv.(0) = "cycles" then
    let g = graph_of_known_cipher Sys.argv.(1) Sys.argv.(2) in
    let c = cycles g in
    let n = List.length c in
    let n_multi = ref 0 in
    iter_multi (fun _ -> incr n_multi) c;
    Printf.fprintf stdout "Il y a %i cycles avant expansion" n;
    if n <= 20 then begin
      Printf.printf ":\n";
      List.iter print_cycle c
    end;
    Printf.fprintf stdout "Il y a %i cycles après expansion\n" !n_multi
