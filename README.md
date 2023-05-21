Task 1

Find functions f1, f2, and f3, such that

fold_left f1 [] [(a1, b1) ; ... ; (an, bn)] for arbitrary ai, bi computes the list [(b1, a1); ... ; (bn, an) ]

fold_left f2 [] [a_0 ; ... ; a_{n−3} ; a_{n−2}; a_{n−1}; a_n] for arbitrary elements a_i computes the list [a_n; a_{n−2} ; ... ; a_0 ; ... ; a_{n−3} ; a_{n−1}]

fold_left f3 (fun _ -> 0) [(k1 , v1) ; ... ; (kn, vn) ] computes a function g such that g(ki) = vi for all 1 ≤ i ≤ n. The k's are assumed to be pairwise distinct.


Task 2

Rewrite the following functions in a tail-recursive form:

   let rec map f = function
      | [] -> []
      | x :: xs -> f x :: map f xs


   let rec replicate n x =
      if n < 1 then [] else x :: replicate (n-1) x


Task 3

Implement a mapping function that maps a function over a lazy list. Implement it both for custom and OCaml lazy list variants. Call them respectively map_over_custom_llist and map_over_ocaml_llist.

Task 4

Implement a merging function that combines two sorted lazy lists.

The idea of merging two lists: merge [1;4;6;7;8; ... ] [1;2;3;4;10; ... ] = [1;1;2;3;4;4;6;7;8;10; ... ]

Implement the function both for custom and OCaml lazy list variants. Call them respectively merge_custom_llists and merge_ocaml_llists.

Task 5

Implement a function that drops duplicates from a sorted lazy list.

Implement it both for custom and OCaml lazy list variants. Call them respectively drop_dupl_custom_llist and drop_dupl_ocaml_llist.

Task 6

Implement a function hamming that lazily computes the infinite sequence of Hamming numbers (i.e., all natural numbers whose only prime factors are 2, 3, and 5), e.g., hamming = [1;2;3;4;5;6;8;9;10;12;15;16;18;20; ... ]

Implement it both for custom and OCaml lazy list variants. Call them respectively hamming_custom_llist and hamming_ocaml_llist.