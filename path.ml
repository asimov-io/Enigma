open Symbol

type t = {head: sym; rev_body: sym list; members: bool array}

let compare (p1: t) (p2: t) : int =
  let cmp = fun s1 s2 -> to_int s1 - to_int s2 in
  List.compare cmp p1.rev_body p2.rev_body

let source (p: t) : sym = p.head

let rev_path (p: t) : sym list = p.rev_body
  
let mem (p: t) (s: sym) : bool = p.members.(to_int s)

let singleton (s: sym) : t =
  {head = s;
  rev_body = [s];
  members = let m = Array.make nb_syms false in m.(to_int s) <- true; m}
  
let snoc (p: t) (s: sym) : t =
  {head = p.head;
  rev_body = s :: p.rev_body;
  members = let m = Array.copy p.members in m.(to_int s) <- true; m}
