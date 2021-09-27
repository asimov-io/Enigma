type cycle

val graph_of_known_cipher : string -> string -> Graph.t

val cycles : Graph.t -> cycle list

val print_cycle : cycle -> unit

val itere : (cycle -> 'a) -> cycle list -> unit

val itere_multi : (cycle -> 'a) -> cycle list -> unit