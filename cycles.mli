type cycle

val graph_of_known_cipher : string -> string -> Graph.t

val cycles : Graph.t -> cycle list

val print_cycle : cycle -> unit

val iter_fusion : (cycle -> 'a) -> cycle list -> unit

val iter_multi : (cycle -> 'a) -> cycle list -> unit