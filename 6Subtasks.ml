(* Task 1 *)


let f1 acc el = acc@[((snd el, fst el))];;

let reorder l = 
  let i = (List.length l) - 1 in
  let rec generateIndices index subtractor op acc = 
    if index mod 2 == 0 then
      if index - subtractor == 0 then generateIndices (index) (index-1) false (acc@[index-subtractor]) else
        if  subtractor < 0 then acc
        else if op=true then
          generateIndices (index) (subtractor + 2) true (acc@[(index - subtractor)]) 
        else
          generateIndices (index) (subtractor - 2) false (acc@[(index - subtractor)])
    else 
      if List.length acc = i + 1 then acc else
        if index - subtractor  < 0  then generateIndices (index) (index - 2) false (acc@[0])
        else 
          if op = true then 
            generateIndices (index) ( subtractor + 2) true (acc@[index - subtractor])
        else
        generateIndices (index) (subtractor - 2) false (acc@[index - subtractor]) in
        generateIndices i 0 true []

let f2 acc el = 
  let rec getAccLength acc l = match acc with 
  | [] -> l
  | x::xs -> getAccLength xs (l+1)
  in 
  let length = getAccLength acc 0
  in 
  if length mod 2 == 1 then acc@[el] else el::acc


let f3 acc el = fun givenChar -> if givenChar = fst el then snd el else acc givenChar

(* 
This f3 function is taking an accumulator which is a funciton itself and it is executing
the anonymous function which takes the argument of 'char' and returns the result based on 
the 'mapping' concept, which means that if the char(as key) is equal to the any first element of the 
  given tuple which 'el', then it is going to return the second element of the given tuple that our
'char' is 'mapped' to, but if it can't find it, it just returns (fun _ -> 0) applied to a(char)

in example:
  g 'a' -: int = 3;;
  g 'z' -: int = -9;;
  g 'b' -: int = 0;;

*)
let testing_fs () =
  let l =
    [
      __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                     [(2,1); (4,3); (6,5)]);
      __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                     ['g';'e';'c';'a';'b';'d';'f']);
      __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                             [('a',3); ('z', -9); ('d', 18)] in
                   (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
  else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 2 *)


let rec map f = function
| [] -> []
| x :: xs -> f x :: map f xs


let rec replicate n x =
if n < 1 then [] else x :: replicate (n-1) x

(* in tail recursive form *)

let map_tr func lst = 
  let rec helper l acc = match l with
  |[] -> acc
  | x::xs -> helper xs (acc@[(func x)])
in helper lst [];;


let replicate_tr times el = 
  let rec helper number value acc = 
    if number == 0 then acc
    else if number < 0 then []
    else
      helper (number - 1) (value) (value::acc)
    in helper times el [];;


    let test_tr_llist () =
      let l =
        [
          __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
          __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
          __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
          __LINE_OF__ (replicate_tr (-3) "a" = [])
        ] in
      let result = List.fold_left (&&) true (List.map snd l) in
      if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
      else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
            (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* Task 3 *)


type 'a custom_llist = (unit -> 'a custom_cell) 
and 'a custom_cell = 
  |NilC 
  |ConsC of ('a * 'a custom_llist)

type 'a ocaml_llist = 'a ocaml_cell Lazy.t 
and 'a ocaml_cell = 
  |NilO
  |ConsO of ('a * 'a ocaml_llist)


(* mapping functions  *)

let  map_over_custom_llist func l= 
  let rec helper lst = match lst () with
  | NilC -> fun () -> NilC
  | ConsC(x, xs) -> fun () -> ConsC(func x, helper (xs))
  in helper l 
  

let map_over_ocaml_llist func l= 
  let rec helper_mapper lst = match Lazy.force lst with
    |NilO -> lazy NilO 
    |ConsO(x, xs) -> lazy (ConsO(func x, helper_mapper xs))
  in (helper_mapper l) 



let rec from_to_custom from to_ step =
  if from <= to_
  then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
  else fun () -> NilC

  
let rec print_custom_llist n c_list =
if n != 0
then match c_list () with
   | NilC -> print_string "Nil\n"
   | ConsC (h, t) ->
      Printf.printf "%d, " h;
      print_custom_llist (n-1) t
else print_string "...\n"

let rec custom_llist_to_string n c_list =
if n != 0
then match c_list () with
| NilC -> "Nil"
| ConsC (h, t) ->
   string_of_int h ^ ", " ^
     custom_llist_to_string (n-1) t
else "..."

let rec from_to_ocaml from to_ step =
  if from <= to_
  then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
  else lazy NilO

let rec print_ocaml_llist n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> print_string "Nil\n"
| ConsO (h, t) ->
   Printf.printf "%d, " h;
   print_ocaml_llist (n-1) t
else print_string "...\n"

let rec ocaml_llist_to_string n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> "Nil"
| ConsO (h, t) ->
   string_of_int h ^ ", " ^
     ocaml_llist_to_string (n-1) t
else "..."


let test_map_llist () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> x+1) (from_to_custom 0 5 1)) =
                     "1, 2, 3, 4, 5, 6, Nil");
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> x+1) (from_to_custom 6 5 1)) =
                     "Nil");
       __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                      "1, 2, 3, 4, 5, 6, Nil");
        __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                       "Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* Task 4 *)


let mergeTwoLists l1 l2 = 
  let (newL1, newL2) = 
  let innerFunc l1 l2 = if List.length l1 <= List.length l2 then
    let temp =  l2
  in let newL1 = temp in
  let newL2 = l1
    in (newL1, newL2)
  else
    (l1, l2)
in 
innerFunc l1 l2 in 
  let rec h1 l1 l2 acc = match l1 with 
  |[] -> acc
  | x::xs -> 
   match l2 with
    | [] -> h1 xs [] (acc@[x])
    | y::ys -> h1 xs ys (acc@[x]@[y])

in h1 newL1 newL2 [] 
   

let merge_custom_llists l1 l2 = 
    let rec getSizeOfLL l acc = match l () with 
  | NilC -> acc
  | ConsC(x, xs) -> (getSizeOfLL (xs) (acc +1))
in 

let (newL1, newL2) = 
  let innerFunc l1 l2 = if getSizeOfLL l1 0 < getSizeOfLL l2 0 then  
      let newL1 = l2 in
      let newL2 = l1
        in (newL1, newL2)
  else
    (l1, l2)
  in innerFunc l1 l2
in
  let rec helper givenL1 givenL2 = match givenL1 () with
  | NilC -> fun () -> NilC
  | ConsC(x, xs) ->  
    if getSizeOfLL givenL2 0 != 0 then
      match givenL2 () with 
      |NilC -> helper (xs) (xs)
      |ConsC(y, ys) -> 
        if x < y then
        fun () -> ConsC(x, helper xs givenL2)
        else
        fun () -> ConsC(y, helper givenL1 ys)
    else
      fun () -> ConsC(x, helper xs givenL2)
    in
     (helper newL1 newL2);;



let merge_ocaml_llists l1 l2 = 
  let rec getSizeOfLL l acc = match Lazy.force l with 
  | NilO -> acc
  | ConsO(x, xs) -> (getSizeOfLL (xs) (acc +1))
in 

let (newL1, newL2) = 
  let innerFunc l1 l2 = if getSizeOfLL l1 0 < getSizeOfLL l2 0 then  
      let newL1 = l2 in
      let newL2 = l1
        in (newL1, newL2)
  else
    (l1, l2)
  in innerFunc l1 l2
in
  let rec helper givenL1 givenL2 = match Lazy.force givenL1 with
  | NilO -> lazy NilO
  | ConsO(x, xs) ->  
    if getSizeOfLL givenL2 0 != 0 then
      match Lazy.force givenL2 with
      |NilO -> helper (xs) (xs)
      |ConsO(y, ys) -> 
        if x < y then
          lazy (ConsO(x, helper xs givenL2))
          else
         lazy (ConsO(y, helper givenL1 ys))
      else
        lazy (ConsO(x, helper xs givenL2))
      in
  helper newL1 newL2;;;;

let test_merge_llists () =
  let l =
    [
       __LINE_OF__ (custom_llist_to_string 13
        (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil"); 
      __LINE_OF__ (ocaml_llist_to_string 13
        (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil") 
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



(* Task 5 *)


let drop_dupl_custom_llist l =
  let rec helper acc lst =
    match lst () with
    | NilC -> fun () -> NilC
    | ConsC (x, xs) ->
        if List.mem x acc then
          helper acc xs
        else
          fun () -> ConsC (x, helper (x::acc) xs)
  in
  helper [] l

let drop_dupl_ocaml_llist l = 
  let rec helper acc lst = 
    match Lazy.force lst with
    | NilO -> lazy NilO
    | ConsO(x, xs) -> 
      if List.mem x acc then 
        helper acc xs
      else
      lazy (ConsO(x, helper (x::acc) xs))
    in helper [] l


let test_drop_dupl_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 13
                     (drop_dupl_custom_llist
                        (merge_custom_llists (from_to_custom 0 5 1)
                           (from_to_custom 0 5 2))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (drop_dupl_custom_llist
                        (merge_custom_llists (from_to_custom 0 5 1)
                           (from_to_custom 6 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (drop_dupl_ocaml_llist
                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                           (from_to_ocaml 0 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (drop_dupl_ocaml_llist
                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                           (from_to_ocaml 6 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* Task 6 *)


let hamming_custom  =  
  let rec isPrimeFactorOf235 n =
    if n mod 2 == 0 then isPrimeFactorOf235 (n/2)
    else if n mod 3 == 0 then isPrimeFactorOf235 (n/3) else if
      n mod 5 == 0 then isPrimeFactorOf235 (n/5) else n = 1
in
  let rec generator x = 
    if isPrimeFactorOf235 (x) then fun () -> ConsC(x, generator (x+1))
    else 
      generator(x+1)
    in generator 1
  

let hamming_ocaml = 
  let rec isPrimeFactorOf235 n =
  if n mod 2 == 0 then isPrimeFactorOf235 (n/2)
  else if n mod 3 == 0 then isPrimeFactorOf235 (n/3) else if
    n mod 5 == 0 then isPrimeFactorOf235 (n/5) else n = 1
in
let rec generator x = 
    if isPrimeFactorOf235 (x) then lazy (ConsO(x, generator (x+1)))
    else generator(x+1)
  in generator 1


let test_hamming_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 14 hamming_custom =
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
      __LINE_OF__ (custom_llist_to_string 20 hamming_custom = 
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
      __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml =
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
      __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml = 
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
  else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

        