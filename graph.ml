type sym = Symbol.sym


module Positions = Set.Make(Int)

type t = Positions.t array array

let create () : t = Array.make_matrix Symbol.nb_syms Symbol.nb_syms (Positions.empty)

let add_edge (g: t) (s1: sym) (s2: sym) (i: int) : unit =
  let i1 = Symbol.to_int s1 in let i2 = Symbol.to_int s2 in
  g.(i1).(i2) <- Positions.add i g.(i1).(i2);
  g.(i2).(i1) <- Positions.add i g.(i2).(i1)

let get_edge (g: t) (s1: sym) (s2: sym) : Positions.t =
  let i1 = Symbol.to_int s1 in let i2 = Symbol.to_int s2 in
  g.(i1).(i2)

let fold_over_connected (g: t) (f: 'a -> sym -> 'a) (x: 'a) (s: sym) : 'a =
  let i = Symbol.to_int s in
  let auxi = fun j -> (fun set -> if Positions.is_empty set then None else Some(j)) in
  let neigh = List.mapi auxi (Array.to_list g.(i)) in
  let filtered = List.filter_map (fun o -> match o with | None -> None | Some i -> Some (Symbol.of_int i)) neigh in
  List.fold_left f x filtered

let rec print_path (g: t) (o: Pervasives.out_channel) (l: sym list) : unit = match l with
  | [] -> ()
  | [h] -> Printf.fprintf o "%c\n" (Symbol.to_char h)
  | h1 :: h2 :: t -> let i1 = Symbol.to_int h1 in let i2 = Symbol.to_int h2 in
    Printf.fprintf o "%c -{ " (Symbol.to_char h1);
    print_set g.(i1).(i2) o;
    Printf.fprintf o "}-> ";
    print_path g o (h2 :: t)

and print_set (set: Positions.t) (o: Pervasives.out_channel) : unit = 
  Positions.iter (fun i -> Printf.fprintf o "%i " i) set