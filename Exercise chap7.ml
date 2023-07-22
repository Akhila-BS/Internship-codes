(*EXERCISE 7 *)

(*chap7.1)
   Define an OCaml record type to represent student names and GPAs. It should be possible to mutate the value of a student’s GPA. Write an expression defining a student with name "Alice" and GPA 3.7. Then write an expression to mutate Alice’s GPA to 4.0.*)


   type student = {name: string; mutable gpa: float}
   let alice = {name = "Alice"; gpa = 3.7}
  let () = alice.gpa <- 4.0
  (*chap7.2
     Give OCaml expressions that have the following types. Use utop to check your answers.
  bool ref
  int list ref
  int ref list*)
  let (_ : bool ref) = ref false
   let (_ : int list ref) = ref [6;8]
   let (_ : int ref list) = [ref 2; ref 8]
   (*output:- : bool ref = {contents = false}
      - : int list ref = {contents = [6; 8]}
      - : int ref list = [{contents = 2}; {contents = 8}]*)
  (*chap7.3
     Define a reference to a function as follows:
  let inc = ref (fun x -> x + 1)
  Write code that uses inc to produce the value 3110.
  *)
  
  
  let inc = ref (fun x -> x + 1)
  let exercise =
    let inc = ref (fun x -> x + 1) in
    !inc 3109
   (*output:
      val inc : (int -> int) ref = {contents = <fun>}
  val exercise : int = 3110*) 
  
  (*chap7.4
     The C language and many languages derived from it, such as Java, has an addition assignment operator written a += b and meaning a = a + b. Implement such an operator in OCaml; its type should be int ref -> int -> unit. Here’s some code to get you started:
  let ( +:= ) x y = ...*)
  let (+:=) x y =
  x := !x + y
  let x = ref 5;;
   x +:= 3;;
  !x;
  
  (output: int = 8)
  
  (*chap7.5
     Define x, y, and z as follows:
  
  let x = ref 0
  let y = x
  let z = ref 0
  Predict the value of the following series of expressions:
  
  # x == y;;
  # x == z;;
  # x = y;;
  # x = z;;
  # x := 1;;
  # x = y;;
  # x = z;;
  Check your answers in utop.*)
  
  let _ =
    let x = ref 0 in
    let y = x in
    let z = ref 0 in
    assert (x == y);
     assert (not (x == z));
     assert (x = y);
     assert (x = z);
     x := 1;
     assert (x = y);
     assert (not (x = z));;
  
  
  
  
    (*output:x == y will be true.
  x == z will be false.
  x = y will be true.
  x = z will be true.
  x := 1 will update the value of x to 1.
  x = y will be true.
  x = z will be false.*)
  
  (*chap7.6
     The Euclidean norm of an n
  -dimensional vector x=(x1,…,xn)
   is written |x|
   and is defined to be
  
  x21+⋯+x2n−−−−−−−−−−√.
  Write a function norm : vector -> float that computes the Euclidean norm of a vector, where vector is defined as follows:
  
  (* AF: the float array [| x1; ...; xn |] represents the
   *     vector (x1, ..., xn)
   * RI: the array is non-empty *)
  type vector = float array
  Your function should not mutate the input array. Hint: although your first instinct might be to reach for a loop, instead try to use Array.map and Array.fold_left or Array.fold_right.*)
  
  type vector = float array
  let norm v =
    sqrt (Array.fold_left (fun acc x -> acc +. x ** 2.) 0. v)
    let norm' v =
     v
     |> Array.map (fun x -> x * 2.)  ( square each element *)
     |> Array.fold_left (+.) 0.       (* sum all elements *)
     |> sqrt
  (*output:type vector = float array
  val norm : float array -> float = <fun>
  val norm' : float array -> float = <fun>*)
  
  (*chap7.7
     Every vector can be normalized by dividing each component by |x|
  ; this yields a vector with norm 1:
  
  (x1|x|,…,xn|x|)
  Write a function normalize : vector -> unit that normalizes a vector “in place” by mutating the input array. Here’s a sample usage:
  
  # let a = [|1.; 1.|];;
  val a : float array = [|1.; 1.|]
  
  # normalize a;;
  - : unit = ()
  
  # a;;
  - : float array = [|0.7071...; 0.7071...|]
  Hint: Array.iteri.*)
  
  let normalize v =
    let n = norm v in 
    Array.iteri (fun i x -> v.(i) <- x /. n) v
    (*output:
       val normalize : float array -> unit = <fun>*)
    
  (*chap7.8
     Modify your implementation of norm to use a loop. Here is pseudocode for what you should do:
  
  initialize norm to 0.0
  loop through array
    add to norm the square of the current array component
  return sqrt of norm*)     
  let norm_loop v =
    let n = ref 0.0 in
    for i = 0 to Array.length v - 1 do
      n := !n +. (v.(i) ** 2.)
    done;
    sqrt !n
    (output:val norm_loop : float array -> float = <fun>)
  
    (*chap7.9
       Modify your implementation of normalize to use a loop.*)
  
       let fact_loop n =
        let ans = ref 1 in
        for i = 1 to n do
          ans := !ans * i
        done;
        !ans
        (output:val fact_loop : int -> int = <fun>)
   (*chap7.10
      The Array module contains two functions for creating an array: make and init. make creates an array and fills it with a default value, while init creates an array and uses a provided function to fill it in. The library also contains a function make_matrix for creating a two-dimensional array, but it does not contain an analogous init_matrix to create a matrix using a function for initialization.
  
  Write a function init_matrix : int -> int -> (int -> int -> 'a) -> 'a array array such that init_matrix n o f creates and returns an n by o matrix m with m.(i).(j) = f i j for all i and j in bounds.
  
  See the documentation for make_matrix for more information on the representation of matrices as arrays.*)     
     
  let init_matrix n o f =
    Array.init n (fun i -> Array.init o (fun j -> f i j))
    (*output:
       val init_matrix : int -> int -> (int -> int -> 'a) -> 'a array array = <fun>*)