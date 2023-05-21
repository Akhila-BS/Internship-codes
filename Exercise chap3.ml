(*****CHAPTER 3*****)

(*Q1*)
(*1.Construct a list that has the integers 1 through 5 in it. Use the square bracket notation for lists.
  2.Construct the same list, but do not use the square bracket notation. Instead use :: and [].
  3.Construct the same list again. This time, the following expression must appear in your answer: [2; 3; 4]. 
    Use the @ operator, and do not use ::.*)


    let lst=[1;2;3;4;5];;
    let lst=1::2::3::4::5::[];;
    let lst=[1]@[2;3;4]@[5];;


(*OUTPUT:
   1. val lst : int list = [1; 2; 3; 4; 5]
   2. val lst : int list = [1; 2; 3; 4; 5]
   3. val lst : int list = [1; 2; 3; 4; 5]*)    

(*Q2*)
(*Write a function that returns the product of all the elements in a list. The product of all the elements of an empty list is 1.*)


let rec pdt lst=
match lst with
|[]->1
|h::t->h*pdt t;; 

pdt[1;2;3];;

(*OUTPUT:
  int = 6 *)


(*Q3*)
(*Write a function that concatenates all the strings in a list. The concatenation of all the strings in an empty list is the empty string "".*)

let rec conc lst =
match lst with
|[]->""
|h::t->h^conc t;;

conc ["Hello";" ";"World"];;
conc [];;

(*OUTPUT:
   string = "Hello World" 
   string = "" *)


(*Q4*)
(*Unit test the function product that you wrote in an exercise above.*)


let rec product lst =
  match lst with
  | [] -> 1
  | hd :: tl -> hd * product tl;;

(* Unit tests *)
let () =
  (* Test case 1: Non-empty list *)
  let test_case_1 = 
    let input = [1; 2; 3; 4; 5] in
    let expected_output = 120 in
    let actual_output = product input in
    if actual_output = expected_output then
      print_endline "Test case 1 passed"
    else
      print_endline "Test case 1 failed"
    in
  (* Test case 2: Empty list *)
  let test_case_2 =
    let input = [] in
    let expected_output = 1 in
    let actual_output = product input in
    if actual_output = expected_output then
      print_endline "Test case 2 passed"
    else
      print_endline "Test case 2 failed"
    in
  (* Run the test cases *)
  let () = 
    test_case_1;
    test_case_2;







(*Q5*)
(*Using pattern matching, write three functions, one for each of the following properties. Your functions should return true if the input list has the property and false otherwise.
     1.The list’s first element is "bigred"
     2.The list has exactly two or four elements; do not use the length function
     3.The first two elements of the list are equal*)


     let  a =
       match ["bigred";"yellow"]with
     | [] -> "false"
     | h :: t -> h  ;;
     


     let b = function
      | _ :: _ :: [] -> true
      | _ :: _ :: _ :: _ :: [] -> true
      | _ -> false;;
     b [1;2;3];;
     b [1; 2; 3; 4];; 
     b [1;2];;


     let c=function
     |a::b::_-> a=b
     |_-> false;;
     c [1; 1; 2; 3];; 
     c [4; 5];;  
     c [2; 2; 2];; 



(*OUTPUT:
   1.val a : string = "bigred"
   2.bool = false
     bool = true
     bool = true
   3.bool = true
     bool = false
     bool = true*)



(*Q6*)     
(*Consult the List standard library to solve these exercises:
     1. Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the list has fewer than five elements, return 0. Hint: List.length and List.nth.
     2. Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with Stdlib.compare as its first argument, and List.rev.*)

 let a lst=
 if (List.length lst)>=5 then List.nth lst 4
 else 0;;
a[1;2;3;4;5;6;7];;
a[1;2;3];;



let b lst=
List.rev (List.sort Stdlib.compare lst);;
b[1;2;3;4;5;6];;
b ["hello"; "apple"; "cherry"];; 

(*OUTPUT:
   1. int = 5
      int = 0
   2. int list = [6; 5; 4; 3; 2; 1] 
      string list = ["hello"; "cherry"; "apple"]  *)



(*Q7*)
(*Write a couple OUnit unit tests for each of the functions you wrote in the previous exercise.*)
 
let get_fifth_element lst=
 if (List.length lst)>=5 then List.nth lst 4
 else 0;;

open OUnit2
let test_get_fifth_element _ =
  assert_equal 0 (get_fifth_element []);
  assert_equal 0 (get_fifth_element [1;2;3;4]);
  assert_equal 1 (get_fifth_element [1;2;3;4;5;6;7])
let suite=
"test_suite" >:::
["test_get_fifth_element">:: test_get_fifth_element;
]
let ()=
run_test_tt_main suite;;


 (*Q8*)
 (*1. Write a function that returns the last element of a list. Your function may assume that the list is non-empty. Hint: Use two library functions, and do not write any pattern matching code of your own.
   2. Write a function any_zeroes : int list -> bool that returns true if and only if the input list contains at least one 0. Hint: use one library function, and do not write any pattern matching code of your own.*)

  

       let get_last_element lst =
        List.hd (List.rev lst);;
        get_last_element [1; 2; 3; 4; 5];;


       let any_zeros lst = 
        List.exists (fun x -> x = 0) lst;;
        any_zeros [1;2;0;3;4];;
        any_zeros [1;3;4;5];;

(*OUTPUT:
   1. int = 5
   2. bool = true 
      bool = false*)



(*Q9*)   
(*1. Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst. If lst has fewer than n elements, return all of them.
  2. Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst. If lst has fewer than n elements, return the empty list.*)

  let rec take n lst =
    if n = 0 then [] else match lst with
      | [] -> []
      | x :: xs -> x :: take (n - 1) xs;;
    take 3 [1; 2; 3; 4; 5];;

    let rec drop n lst =
      if n = 0 then lst else match lst with
        | [] -> []
        | x :: xs -> drop (n - 1) xs;;
        drop 3 [1; 2; 3; 4; 5];;
(*OUTPUT:
   1.int list = [1; 2; 3]
   2.int list = [4; 5] *)


(*Q10*)
(*Revise your solutions for take and drop to be tail recursive, if they aren’t already. Test them on long lists with large values of n to see whether they run out of stack space. To construct long lists, use the -- operator from the lists section.*)

 let rec take_rev n xs acc = 
    if n = 0 then acc else match xs with
      | [] -> acc
      | x :: xs' -> take_rev (n - 1) xs' (x :: acc);;
      take_rev 3 [1;2;3;4;5] [];;

(*OUTPUT:
   int list = [3; 2; 1]*)


(*Q11*)
(*Write a function is_unimodal : int list -> bool that takes an integer list and returns whether that list is unimodal.*)


let rec is_mon_dec = function
  | [] | [_] -> true
  | h1 :: (h2 :: t2 as t) -> 
    h1 >= h2 && is_mon_dec t;;


let rec is_mon_inc_then_dec = function
  | [] | [_] -> true
  | h1 :: (h2 :: t2 as t) as lst -> 
    if h1 <= h2 then is_mon_inc_then_dec t else is_mon_dec lst;;

let is_unimodal lst = 
  is_mon_inc_then_dec lst;;

  is_unimodal [1;2;3;4;5];;

(*OUTPUT:
   bool = true*)


(*Q12*)
(*Write a function powerset : int list -> int list list that takes a set S represented as a list and returns the set of all subsets of S. The order of subsets in the powerset and the order of elements in the subsets do not matter.*)

let rec powerset = function
  | [] -> [ [] ]
  | x :: s -> let p = powerset s in
    List.map (List.cons x) p @ p;;

 powerset [1; 2; 3];;

(*OUTPUT:
   int list list = [[1; 2; 3]; [1; 2]; [1; 3]; [1]; [2; 3]; [2]; [3]; []]*)


(*Q13*)
(*Write a function print_int_list : int list -> unit that prints its input list, one number per line. For example, print_int_list [1; 2; 3] should result in this output:*)

let rec print_int_list = function
  | [] -> ()
  | hd :: tl -> print_endline (string_of_int hd); print_int_list tl;;

  print_int_list  [1; 2; 3];;

(*OUTPUT:
   1
   2
   3*)



(*Q14*)
(*Write a function print_int_list' : int list -> unit whose specification is the same as print_int_list. Do not use the keyword rec in your solution, but instead to use the List module function List.iter.*)

let print_int_list' lst =
  List.iter (fun x -> print_endline (string_of_int x)) lst;;
print_int_list [1;2;3];;

(*OUTPUT:
  1
  2
  3 *)



(*Q15*)
(*Give OCaml expressions that have the following types:
     1. student
     2. student -> string * string (a function that extracts the student’s name)
     3. string -> string -> float -> student (a function that creates a student record)*)


type student = { first_name : string ; last_name : string ; gpa : float };;


 let s =
  { first_name = "Ezra"; last_name = "Cornell"; gpa = 4.3 };;


  let make_stud first last g =
    { first_name = first; last_name = last; gpa=g };;
 



(*Q16*)
(*1. Define the type pokemon to be a record with fields name (a string), hp (an integer), and ptype (a poketype).
  2. Create a record named charizard of type pokemon that represents a Pokémon with 78 HP and Fire type.
  3. Create a record named squirtle of type pokemon that represents a Pokémon with 44 HP and Water type.*)     

type pokemon = { name : string ; hp : int ; ptype : poketype };;

let charizard = { name = "charizard"; hp = 78; ptype = Fire };;

let squirtle = { name = "squirtle"; hp = 44; ptype = Water };;


(*Q17*)
(*1. Write a function safe_hd : 'a list -> 'a option that returns Some x if the head of the input list is x, and None if the input list is empty.
  2. Also write a function safe_tl : 'a list -> 'a list option that returns the tail of the list, or None if the list is empty.*)

    let safe_hd = function
    | [] -> None
    | h::_ -> Some h;;


    let safe_tl = function
  | [] -> None
  | _::t -> Some t;;



(*Q18*)  
(*Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP.*)

let rec max_hp = function
  | [] -> None
  | poke1::t -> begin
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2)
    end;;




(*Q19*)
(*date before*)

let is_before date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2);;


(*Q20*)
(*Write a function earliest : (int*int*int) list -> (int * int * int) option. It evaluates to None if the input list is empty, and to Some d if date d is the earliest date in the list.*)

  let rec earliest = function
  | [] -> None
  | d1::t -> begin
      match earliest t with
      | None -> Some d1
      | Some d2 -> Some (if is_before d1 d2 then d1 else d2)
    end;;


(*Q21*)
(*Use the functions insert and lookup from the section on association lists to construct an association list that maps the integer 1 to the string “one”, 2 to “two”, and 3 to “three”. Lookup the key 2. Lookup the key 4.*)

let rec lookup k = function
  | [] -> None
  | (k',v)::t -> if k=k' then Some v else lookup k t;;

let dict = insert 3 "three" (insert 2 "two" (insert 1 "one" []));;
let some_two = lookup 2 dict;;
let none = lookup 4 dict;;


(*Q22*)
(*Define a variant type suit that represents the four suits, ♣ ♦ ♥ ♠, in a standard 52-card deck. All the constructors of your type should be constant.

Define a type rank that represents the possible ranks of a card: 2, 3, …, 10, Jack, Queen, King, or Ace. There are many possible solutions; you are free to choose whatever works for you. One is to make rank be a synonym of int, and to assume that Jack=11, Queen=12, King=13, and Ace=1 or 14. Another is to use variants.

Define a type card that represents the suit and rank of a single card. Make it a record with two fields.

Define a few values of type card: the Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of Spades.

*)

type suit = Hearts | Spades | Clubs | Diamonds

type rank = Number of int | Ace | Jack | Queen | King

type card = { suit: suit; rank: rank }

let ace_of_clubs : card = { suit = Clubs; rank = Ace };;
let queen_of_hearts : card = { suit = Hearts; rank = Queen };;
let two_of_diamonds : card = { suit = Diamonds; rank = Number 2 };;
let seven_of_spades : card = { suit = Spades; rank = Number 7 };;


(*Q23*)
(*For each pattern in the list below, give a value of type int option list that does not match the pattern and is not the empty list, or explain why that’s impossible.*)

(* (Some x)::tl *)
let lst1 : int option list = [None; Some 0];;
(* [Some 3110; None] *)
let lst2 : int option list = [Some 3410; None];;
(* [Some x; _] *)
let lst3 : int option list = [None; Some 0];;
(* h1::h2::tl *)
let lst4 : int option list = [Some 0];;

(* h :: tl *)
(* Impossible. This matches any list of non-zero length regardless of type.
 * The only way to NOT match this expression is the empty list []. *)


 (*Q24*)
 (*quadrant*)

 type quad = I | II | III | IV
type sign = Neg | Zero | Pos

(** [sign x] is [Zero] if [x = 0]; [Pos] if [x > 0]; and [Neg] if [x < 0]. *)
let sign x =
  if x = 0 then Zero
  else if x > 0 then Pos
  else Neg;;

(** [quadrant (x,y)] is [Some q] if [(x, y)] lies in quadrant [q], or [None] if
    it lies on an axis. *)
let quadrant (x,y) =
  match sign x, sign y with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None;;


  (*Q25*)
  (*Rewrite the quadrant function to use the when syntax. You won’t need your helper function from before.*)

  let quadrant_when = function
  | x,y when x > 0 && y > 0 -> Some I
  | x,y when x < 0 && y > 0 -> Some II
  | x,y when x < 0 && y < 0 -> Some III
  | x,y when x > 0 && y < 0 -> Some IV
  | _ -> None;;

(*Q26*)
(*Write a function depth : 'a tree -> int that returns the number of nodes in any longest path from the root to a leaf. For example, the depth of an empty tree (simply Leaf) is 0, and the depth of tree t above is 3. Hint: there is a library function max : 'a -> 'a -> 'a that returns the maximum of any two values of the same type.*)


let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right);;


(*Q27*)
(*let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)*)

  let rec same_shape t1 t2 =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Node (_,l1,r1), Node (_,l2,r2) -> (same_shape l1 l2) && (same_shape r1 r2)
    | _ -> false;;

    (*Q28*)
    (*Write a function list_max : int list -> int that returns the maximum integer in a list, or raises Failure "list_max" if the list is empty.*)

      let rec list_max_safe x = function
      | [] -> x
      | h::t -> list_max_safe (Stdlib.max x h) t;;


      (*Q29*)
      (*Write a function list_max_string : int list -> string that returns a string containing the maximum integer in a list, or the string "empty" (note, not the exception Failure "empty" but just the string "empty") if the list is empty. Hint: string_of_int in the standard library will do what its name suggests.*)

        let list_max_string lst =
          try string_of_int (list_max lst) with
          | Failure _ -> "empty";;


    (*Q30*)
    (*Write two OUnit tests to determine whether your solution to list max exn, above, correctly raises an exception when its input is the empty list, and whether it correctly returns the max value of the input list when that list is nonempty.*)

    open OUnit2

    let test_list_max_exn_empty _ =
      assert_raises (Failure "Empty list") (fun () -> list_max_exn []);;
    
    let test_list_max_exn_nonempty _ =
      let input = [3; 1; 4; 1; 5; 9] in
      let expected_output = 9 in
      assert_equal expected_output (list_max_exn input);;
    
    let suite =
      "list_max_exn_tests" >:::
        [ "test_list_max_exn_empty" >:: test_list_max_exn_empty;
          "test_list_max_exn_nonempty" >:: test_list_max_exn_nonempty ];;
    
    let () =
      run_test_tt_main suite;;
    

   (*Q31*)
   (*Write a function is_bst : ('a*'b) tree -> bool that returns true if and only if the given tree satisfies the binary search tree invariant. An efficient version of this function that visits each node at most once is somewhat tricky to write. Hint: write a recursive helper function that takes a tree and either gives you (i) the minimum and maximum value in the tree, or (ii) tells you that the tree is empty, or (iii) tells you that the tree does not satisfy the invariant. Your is_bst function will not be recursive, but will call your helper function and pattern match on the result. You will need to define a new variant type for the return type of your helper function.*) 
        
    type ('a, 'b) tree =
    | Leaf
    | Node of ('a, 'b) tree * 'a * 'b * ('a, 'b) tree
  
  type 'a bst_result =
    | Empty
    | Invariant of 'a * 'a
    | NotInvariant
  
  let rec min_max_value_tree tree =
    match tree with
    | Leaf -> Empty
    | Node (left, key, _, right) ->
      let left_result = min_max_value_tree left in
      let right_result = min_max_value_tree right in
      match (left_result, right_result) with
      | (Empty, Empty) -> Invariant (key, key)
      | (Empty, Invariant (rmin, rmax)) -> Invariant (key, rmax)
      | (Invariant (lmin, lmax), Empty) -> Invariant (lmin, key)
      | (Invariant (lmin, lmax), Invariant (rmin, rmax)) ->
        if lmax <= key && key <= rmin then Invariant (lmin, rmax)
        else NotInvariant;;
  
  let is_bst tree =
    match min_max_value_tree tree with
    | Invariant _ -> true
    | _ -> false;;

   (*Q32*) 
  (*Modify your definition of quadrant to use polymorphic variants. The types of your functions should become these:*)


  let sign_poly x : [> `Neg | `Pos | `Zero] =
    if x < 0 then `Neg
    else if x = 0 then `Zero
    else `Pos;;
  
  let quadrant (x,y) : [> `I | `II | `III | `IV ] option =
    match sign_poly x, sign_poly y with
    | `Pos, `Pos -> Some `I
    | `Neg, `Pos -> Some `II
    | `Neg, `Neg -> Some `III
    | `Pos, `Neg -> Some `IV
    | _ -> None;;



