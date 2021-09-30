open Symbol

type t = (Symbol.Set.t * int * sym option) Map.t

(* For s in [sym], we have a set that represents the complement of p(s), an int that gives the size of p(s) and a sym option that is Some(s') if s is necessarily associated to some symbol s', and None otherwise. *)

(* !!! WARNING !!! *)
(* Once an association (s, s') is found, the first two component, relative to p(s) (resp. p(s')) are not updated again; we don’t need to access them anymore because the information is stocked in the third component. *)

let top ((): unit) : t =
  Map.make (Symbol.Set.empty, nb_syms, None)


let print_set (set: Symbol.Set.t) (o: Pervasives.out_channel) : unit =
  Printf.fprintf o "{";
  for i = 0 to nb_syms - 1 do
    let c = of_int i in
    if (Symbol.Set.member c set) then 
      Printf.fprintf o "%c," (to_char c)
  done;
  Printf.fprintf o "}"


let print ~(long: bool) (o: out_channel) (b: t) : unit =
  for i = 0 to nb_syms - 1 do
    let s = of_int i in
    let _, _, opt = Map.get b s in
    match opt with
      | Some s' -> Printf.fprintf o "(%c, %c)\n" (to_char s) (to_char s')
      | None -> ()
  done;

  if long then begin
    for i = 0 to nb_syms - 1 do
      let s = of_int i in
      let set, nb, _ = Map.get b s in
      Printf.fprintf o "%c: (" (to_char s); print_set set stdout; Printf.fprintf o ", %i)\n" nb
    done;
    Printf.fprintf o "\n"
    end
  

let possible (b: t) (s1: sym) (s2: sym) : bool =
  let set, _, opt = Map.get b s1 in
  match opt with
    | Some(s) when s = s2 -> true
    | Some(_) -> false
    | None -> let _, _, opt2 = Map.get b s2 in
      if opt2 <> None
      then false
      else not (Symbol.Set.member s2 set)


let possibles (b: t) (s: sym) : sym list =
  fold (fun l s' -> if possible b s s' then s' :: l else l) []


exception Impossible


let find_elem (set: Symbol.Set.t) : sym =
  List.find (fun c -> not (Symbol.Set.member c set)) (List.init nb_syms of_int)


let remove_assoc (b: t) (s1: sym) (s2: sym) : unit =
  let rec remove_from (s1: sym) (s2: sym) : unit =
    let set2, nb2, opt2 = Map.get b s2 in
    let _, _, opt1 = Map.get b s1 in
    match opt2 with
      | Some(s) when s = s1 -> raise Impossible
      | Some(_) -> ()
      | None -> if opt1 <> None then () else
        begin
          if not (Symbol.Set.member s1 set2) then begin
            let set2' = Symbol.Set.add s1 set2 in
            Map.set b s2 (set2', nb2 - 1, opt2);
            if nb2 = 2 then begin
              (* We find a new forced association *)
              let s = find_elem set2' in
              for i = 0 to nb_syms - 1 do
                let x = of_int i in
                if (x <> s && x <> s2) then begin
                  remove_from s2 x; remove_from s x
                end
              done;
              let set, nb, _ = Map.get b s in
              Map.set b s2 (set2', nb2 - 1, Some(s));
              Map.set b s (set, nb, Some(s2))
            end
          end
        end
  in remove_from s1 s2; remove_from s2 s1