val f1 : ('a * 'b) list -> 'b * 'a -> ('a * 'b) list
val reorder : 'a list -> int list
val f2 : 'a list -> 'a -> 'a list
val f3 : ('a -> 'b) -> 'a * 'b -> 'a -> 'b
val testing_fs : unit -> int list
val map : ('a -> 'b) -> 'a list -> 'b list
val replicate : int -> 'a -> 'a list
val map_tr : ('a -> 'b) -> 'a list -> 'b list
val replicate_tr : int -> 'a -> 'a list
val test_tr_llist : unit -> int list
type 'a custom_llist = unit -> 'a custom_cell
and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
type 'a ocaml_llist = 'a ocaml_cell lazy_t
and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)
val map_over_custom_llist : ('a -> 'b) -> 'a custom_llist -> 'b custom_llist
val map_over_ocaml_llist :
  ('a -> 'b) -> 'a ocaml_cell lazy_t -> 'b ocaml_llist
val from_to_custom : int -> int -> int -> int custom_llist
val print_custom_llist : int -> int custom_llist -> unit
val custom_llist_to_string : int -> int custom_llist -> string
val from_to_ocaml : int -> int -> int -> int ocaml_llist
val print_ocaml_llist : int -> int ocaml_cell lazy_t -> unit
val ocaml_llist_to_string : int -> int ocaml_cell lazy_t -> string
val test_map_llist : unit -> int list
val mergeTwoLists : 'a list -> 'a list -> 'a list
val merge_custom_llists :
  'a custom_llist -> 'a custom_llist -> 'a custom_llist
val merge_ocaml_llists :
  'a ocaml_cell lazy_t -> 'a ocaml_cell lazy_t -> 'a ocaml_llist
val test_merge_llists : unit -> int list
val drop_dupl_custom_llist : 'a custom_llist -> 'a custom_llist
val drop_dupl_ocaml_llist : 'a ocaml_cell lazy_t -> 'a ocaml_llist
val test_drop_dupl_llists : unit -> int list
val hamming_custom : int custom_llist
val hamming_ocaml : int ocaml_llist
val test_hamming_llists : unit -> int list
