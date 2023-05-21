(*Q1*)

let double x = 2*x;;
let square x = x*x;;
let twice f x = f (f x);;
let quad = twice double;;
let fourth = twice square;;

(*Q2*)
(*What does the following operator do?
let ( $ ) f x = f x*)

let ( $ ) f x = f x;;
(*val ( $ ) : ('a -> 'b) -> 'a -> 'b = <fun>*)

(*Q3*)
(*What does the following operator do?
let ( @@ ) f g x = x |> g |> f*)

let ( @@ ) f g x = x |> g |> f;;

(*val ( @@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>*)


(*Q4*)
(*Generalize twice to a function repeat, such that repeat f n x applies f to x a total of n times*)
let rec repeat f n x =
  if n = 0 then x else repeat f (n - 1) (f x);;

(*Q5*)
(*Use fold_left to write a function product_left that computes the product of a list of floats. The product of the empty list is 1.0.*)

let product_left lst = List.fold_left ( *. ) 1.0 lst;;

(*Q6*)
(*How terse can you make your solutions to the product exercise? Hints: you need only one line of code for each, and you do not need the fun keyword. For fold_left, your function definition does not even need to explicitly take a list argument. If you use ListLabels, the same is true for fold_right.*)

let product = List.fold_left ( * );; 1
let product = ListLabels.fold_right ~f:( * ) ~init:1;;


(*Q7*)
(*Write a function sum_cube_odd n that computes the sum of the cubes of all the odd numbers between 0 and n inclusive. Do not write any new recursive functions. Instead, use the functionals map, fold, and filter, and the ( -- ) operator (defined in the discussion of pipelining).*)
let rec from i j l =
  if i>j then l
  else from i (j-1) (j::l);;

  let (--) i j =
  from i j [];;

  let sum_cube_odd n =
    let l = 0 -- n in
    let odds_only = List.filter (fun i -> i mod 2 = 1) l in
    let odd_cubes = List.map (fun i -> i * i * i) odds_only in
    List.fold_left (+) 0 odd_cubes;;

   
    (*Q8*)
    (*Rewrite the function sum_cube_odd to use the pipeline operator |>.*)

    let sum_cube_odd_p n =
      0 -- n
      |> List.filter (fun i -> i mod 2 = 1)
      |> List.map (fun i -> i * i * i)
      |> List.fold_left (+) 0;;


    (*Q9*)
    (*Consider writing a function exists: ('a -> bool) -> 'a list -> bool, such that exists p [a1; ...; an] returns whether at least one element of the list satisfies the predicate p. That is, it evaluates the same as (p a1) || (p a2) || ... || (p an). When applied to an empty list, it evaluates to false.*)

    
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t;;

  let exists_fold p l = List.fold_left (fun acc elt -> acc || p elt) false l;;

  
let exists_lib = List.exists;;


(*Q10*)
(*Write a function which, given a list of numbers representing debits, deducts them from an account balance, and finally returns the remaining amount in the balance. Write three versions: fold_left, fold_right, and a direct recursive implementation.*)

let remaining_balance_fold_left balance debits =
  List.fold_left (fun acc debit -> acc - debit) balance debits;;

  let remaining_balance_fold_right balance debits =
    ListLabels.fold_right ~f:(fun debit acc -> acc - debit) ~init:balance debits;;

    let rec remaining_balance_recursive balance debits =
      match debits with
      | [] -> balance
      | debit :: rest -> remaining_balance_recursive (balance - debit) rest;;
    
  
      (*Q11*)
      (*write uncurried versions of these library functions:
List.append
Char.compare
Stdlib.max*)

let uncurried_append (lst,e) = List.append lst e;;

let uncurried_compare (c1,c2) = Char.compare c1 c2;;

let uncurried_max (n1,n2) = Stdlib.max n1 n2;;


(*Q12*)
(*Show how to replace any expression of the form List.map f (List.map g lst) with an equivalent expression that calls List.map only once.*)

let combined_map lst =
  List.map (f@@g) lst;;


  (*Q13*)
(*Write functions that perform the following computations. Each function that you write should use one of List.fold, List.map or List.filter. To choose which of those to use, think about what the computation is doing: combining, transforming, or filtering elements.
Find those elements of a list of strings whose length is strictly greater than 3.
Add 1.0 to every element of a list of floats.
Given a list of strings strs and another string sep, produce the string that contains every element of strs separated by sep. For example, given inputs ["hi";"bye"] and ",", produce "hi,bye", being sure not to produce an extra comma either at the beginning or end of the result string.*)

let at_least_three lst =
  List.filter (fun s -> String.length s > 3) lst;;

  let add_one lst =
    List.map (fun x -> x +. 1.0) lst;;

    let join_with strs sep =
      match strs with
      | [] -> ""
      | x :: xs ->
        List.fold_left (fun combined s -> combined ^ sep ^ s) x xs;;


(*Q14*)
(*Write a function keys: ('a * 'b) list -> 'a list that returns a list of the unique keys in an association list. Since they must be unique, no value should appear more than once in the output list. The order of values output does not matter. How compact and efficient can you make your solution? Can you do it in one line and linearithmic space and time? Hint: List.sort_uniq.*)
    
let keys1 lst =
  List.fold_right
    (fun (k, _) acc -> k :: List.filter (fun k2 -> k <> k2) acc)
    lst
    [];;

    let keys2 lst =
      List.fold_left
        (fun acc (k, _) -> if List.exists ((=) k) acc then acc else k::acc)
        []
        lst;;

        let keys3 lst =
          lst
          |> List.rev_map fst
          |> List.sort_uniq Stdlib.compare;;
    


(*Q15*)
(*Implement a function is_valid_matrix: int list list -> bool that returns whether the input matrix is valid. Unit test the function.*)

let is_valid_matrix = function
  | [] -> false
  | r :: rows ->
    let m = List.length r in
    m > 0 && List.for_all (fun r' -> m = List.length r') rows;;


(*Q16*)
(*mplement a function add_row_vectors: int list -> int list -> int list for the element-wise addition of two row vectors. For example, the addition of [1; 1; 1] and [9; 8; 7] is [10; 9; 8]. If the two vectors do not have the same number of entries, the behavior of your function is unspecified—that is, it may do whatever you like. Hint: there is an elegant one-line solution using List.map2. Unit test the function.*)

let add_row_vectors =
  List.map2 (+);;

 (*Q17*)
 (*Implement a function add_matrices: int list list -> int list list -> int list list for matrix addition. If the two input matrices are not the same size, the behavior is unspecified. Hint: there is an elegant one-line solution using List.map2 and add_row_vectors. Unit test the function.*)
 
 
let add_matrices =
  List.map2 add_row_vectors;;


(*Q18*)
(*Implement a function multiply_matrices: int list list -> int list list -> int list list for matrix multiplication. If the two input matrices are not of sizes that can be multiplied together, the behavior is unspecified. Unit test the function. Hint: define functions for matrix transposition and row vector dot product.*)

let rec transpose ls =
  let rec transpose' acc = function
    | [] | [] :: _ -> List.rev acc
    | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
  in transpose' [] ls

let dot = List.fold_left2 (fun acc x y -> acc + x * y) 0

let multiply_matrices m1 m2 =
  List.map (fun row -> List.map (dot row) (transpose m2)) m1;;
















