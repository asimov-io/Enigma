open Char

type sym = int

let a = 0

let nb_syms = 26

let of_char (c: char) : sym =
  (code c) - 65

let to_char (s: sym) : char =
  chr (s + 65)


let of_int (i: int) : sym = i

let to_int (s: sym) : int = s

let next (s: sym) : sym = (s + 1) mod nb_syms

let (++) (s1: sym) (s2: sym) : sym = (s1 + s2) mod nb_syms

let (--) (s1: sym) (s2: sym) : sym = (s1 - s2 + nb_syms) mod nb_syms 

let iter (f: sym -> 'a) : unit =
  for s = 0 to nb_syms - 1 do
    let _ = f s in ();
  done


let fold (f: 'a -> sym -> 'a) (x: 'a) : 'a =
  let rec aux (acc: 'a) (s: sym) = match s with
    | s when s = nb_syms - 1 -> f acc s
    | _ -> aux (f acc s) (next s)
  in aux x 0

(* Computes a ** b *)
let quick_exp a b =
  let rec aux acc a b = match b with
    | 0 -> acc
    | 1 -> acc * a
    | n when n mod 2 = 0 -> aux acc (a * a) (n / 2)
    | _ -> aux (a * acc) (a * a) (b / 2)
  in aux 1 a b


module Set =
  struct
    type t = int
    let empty = 0

    let member (s: sym) (set: t) : bool =
      (set / (quick_exp 2 (to_int s))) mod 2 = 1
    
    (* let add (s: sym) (set: t) : t =
      let rec aux s set mult rem = match s with
        | 0 -> (2 * (set / 2) + 1) * mult + rem
        | _ -> aux (s -- 1) (set / 2) (2 * mult) (rem + mult * (set mod 2))
      in aux s set 1 0 *)

    let add (s: sym) (set: t) : t =
      if not (member s set) then
        set + quick_exp 2 (to_int s)
      else
        set

    let rec singleton (s: sym) : t =
      quick_exp 2 (to_int s)
end


module Map =
  struct
    type 'a t = 'a array

    let get (tab: 'a t) (s: sym) : 'a = tab.(s)

    let set (tab: 'a t) (s: sym) (v: 'a) : unit = tab.(s) <- v

    let make (v: 'a) : 'a t = Array.make nb_syms v

    let init (f: sym -> 'a) : 'a t = Array.init nb_syms f

    let copy (tab: 'a t) : 'a t = Array.copy tab
    
    let map (f: 'a -> 'b) (tab: 'a t) : 'b t = Array.map f tab

    let inverse (tab: sym t) : sym t =
      let aux (s: sym) : sym =
        let res = ref 0 in
        for i = 0 to nb_syms - 1 do
          if tab.(i) = s then
            res := i
        done;
        !res
      in init aux

    let print_tmap (o: out_channel) (tab: sym t) : unit =
      iter (fun s -> Printf.fprintf o "%c" (to_char (get tab s)))
end